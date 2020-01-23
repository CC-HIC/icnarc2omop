#' Create an OMOP database from ICNARC XML
#'
#' This function instantionates an OHDSI CDM 6 (referred to throughout as OMOP
#' for brevity) database from raw ICNARC XML. In doing so, it creates an
#' extremely spartan instance of OMOP. This behaviour is intentional to short
#' cut the creation of an OMOP database. After the OMOP database has been
#' created, you have the option to add more data directly from more ICNARC XML
#' in the future, or begin to populate the database directly with data from
#' another source.
#'
#' @param project_path the path to a project folder with:
#'  - vocab
#'  - meta
#'  - xml
#' @param nhs_trust a character string with the full name of the trust.
#' @param source_description path of a comprehensive note about the origin and
#'   provenance of the ICNARC XML for example, what systems in the hospital
#'   are used to compile the XML, or is it created by hand. Should point to a
#'   plain text file.
#' @param cdm_version the version of the CDM you are using. Can only be
#'   "6.0.0" at present
#' @param vocabulary_version the version of the vocabulary you are using
#' @param database_name the name of the database you are connecting to
#' @param database_engine the database engine (e.g. sql server)
#' @param host_name host ip address
#' @param port_no port number
#' @param username username to database (must have write privaleges)
#' @param password database password
#' @param sqlite_file a filename if using sqlite
#'
#' @importFrom purrr iwalk
#' @importFrom rlang inform
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#' @importFrom RMySQL MySQL
#' @importFrom odbc odbc
#' @importFrom RSQLite SQLite
#'
#' @return TRUE if completed without errors
#' @export
omopify_xml <- function(project_path,
                        nhs_trust,
                        source_description,
                        cdm_version = "6.0.0",
                        vocabulary_version = "5",
                        database_name = NULL,
                        database_engine = "postgres",
                        host_name = "localhost",
                        port_no = 5432,
                        username = NULL,
                        sqlite_file = NULL) {

  # Check db engine
  database_engine <- tolower(database_engine)
  db_options <- c(
    "sqlite",
    "postgres",
    "mysql"
    )

  if (all(!(database_engine %in% db_options))) {
    rlang::abort(
      glue::glue("{database_engine} is not a valid choice. Please select from:\n
                 {db_options}")
    )
  }

  # Check project folder structure

  project_dirs <- list.dirs(project_path, full.names = FALSE)
  if (!all(c("meta", "xml", "vocab") %in% project_dirs)) {
    print(project_dirs %in% c("meta", "xml", "vocab"))
    rlang::abort("Your folder structure is not correct")
  }

  dir.create(file.path(project_path, "ddl"))

  rlang::inform("Attempting to connect to database")

  if (is.null(sqlite_file)) {

    db_engine <- database_engine %>%
      switch(
        sqlite = NULL,
        postgres = RPostgres::Postgres,
        mysql = RMySQL::MySQL
      )

    ctn <- DBI::dbConnect(
      db_engine(),
      host = host_name,
      port = port_no,
      user = username,
      password = rstudioapi::askForPassword("Please enter your password"),
      dbname = database_name
      )
  } else {
    ctn <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
  }

  rlang::inform("Connection established")

  ## Check exiting table structure
  tbls <- DBI::dbListTables(ctn)

  if (length(tbls) != 0) {
    rlang::abort("You should be connecting to an empty database. Try again.")
  }

  ddl_path <- database_engine %>%
    switch(
      sqlite = NULL,
      postgres = "https://raw.githubusercontent.com/OHDSI/CommonDataModel/master/PostgreSQL/OMOP%20CDM%20postgresql%20ddl.txt",
      mysql = "https://raw.githubusercontent.com/OHDSI/CommonDataModel/master/Sql%20Server/OMOP%20CDM%20sql%20server%20ddl.txt"
    )

  download.file(
    ddl_path,
    destfile = file.path(project_path, "ddl/ddl.txt"))

  qrys <- readr::read_file(file.path(project_path, "ddl/ddl.txt")) %>%
    str_extract_all("(?s)(?<=CREATE TABLE).*?(?=;)") %>%
    magrittr::extract2(1) %>%
    paste0("CREATE TABLE", ., ";")

  qry_result <- purrr::map_lgl(.x = qrys, .f = ~ transact(ctn, .x))

  if (all(qry_result)) {
    rlang::inform("Empty tables have been written successfully")
  } else {
    rlang::abort("Problem writing out tables to database")
  }

  table_names <- sort(DBI::dbListTables(ctn))

  my_cdm <- purrr::imap(
    .x = table_names,
    .f = ~ dplyr::collect(dplyr::tbl(ctn, .x))) %>%
    set_names(table_names)

  rlang::inform("Reading and converting XML")

  # Convert XML to the correct form
  df <- extract_xml(file.path(project_path, "xml"))
  rlang::inform("ICNARC XML parsed successfully")

  # Set up tables according to the CDM 6 Schema
  # Capture tables in list
  rlang::inform("Starting CDM build")

  # VOCABULARIES ====

  rlang::inform("Reading in vocabularies")
  my_vocab <- extract_vocab(vocab_path)
  iwalk(my_vocab, ~ copy_to(
    ctn, .x, name = .y, overwrite = TRUE, temporary = FALSE)
  )

  # STANDARDISED METADATA
  max_date <- max(df$daicu, na.rm = TRUE)

  my_cdm[["cdm_source"]] <-
    setup_cdm_source(
      my_cdm,
      nhs_trust,
      source_description,
      max_date,
      cdm_version,
      vocabulary_version
      )

  my_cdm[["metadata"]] <- setup_metadata(my_cdm)

  # STANDARDISED HEALTH SYSTEM
  my_cdm[["location"]] <- setup_location(my_cdm, df, nhs_trust)
  # Must to location history out of sequence
  my_cdm[["care_site"]] <- setup_care_site(my_cdm, df, nhs_trust)
  my_cdm[["provider"]] <- setup_provider(my_cdm)

  # STANDARDISED CLINICAL DATA
  my_cdm[["person"]] <- setup_person(my_cdm, df)
  my_cdm[["observation_period"]] <- setup_observation_period(my_cdm)

  # Interruption to sequence to population location history
  my_cdm[["location_history"]] <- setup_location_history(my_cdm, df)

  # Continue with original order
  my_cdm[["visit_occurrence"]] <- setup_visit_occurrence(my_cdm, df, nhs_trust)
  my_cdm[["visit_detail"]] <- setup_visit_detail(my_cdm, df)
  my_cdm[["condition_occurrence"]] <- setup_condition_occurrence(my_cdm)
  my_cdm[["drug_exposure"]] <- setup_drug_exposure(my_cdm)
  my_cdm[["procedure_occurrence"]] <- setup_procedure_occurrence(my_cdm)
  my_cdm[["drug_exposure"]] <- setup_drug_exposure(my_cdm)
  my_cdm[["device_exposure"]] <- setup_device_exposure(my_cdm)
  my_cdm[["measurement"]] <- setup_measurement(my_cdm)
  my_cdm[["note"]] <- setup_note(my_cdm)
  my_cdm[["note_nlp"]] <- setup_note_nlp(my_cdm)
  my_cdm[["survey_conduct"]] <- setup_survey_conduct(my_cdm)
  my_cdm[["observation"]] <- setup_observation(my_cdm)
  my_cdm[["specimen"]] <- setup_specimen(my_cdm)
  my_cdm[["fact_relationship"]] <- setup_fact_relationship(my_cdm, nhs_trust)

  # STANDARDISED HEALTH ECONOMICS
  my_cdm[["payer_plan_period"]] <- setup_payer_plan_period(my_cdm)
  my_cdm[["cost"]] <- setup_cost(my_cdm)

  # STANDARDISED DERIVED ELEMENTS

  my_cdm[["drug_era"]] <- setup_drug_era(my_cdm)
  my_cdm[["dose_era"]] <- setup_dose_era(my_cdm)
  my_cdm[["condition_era"]] <- setup_condition_era(my_cdm)

  # RESULTS SCHEMA

  my_cdm[["cohort_tbl"]] <- setup_cohort(my_cdm)
  my_cdm[["cohort_definition_tbl"]] <- setup_cohort_definition(my_cdm)

  rlang::inform("Finished CDM build")

  rlang::inform("Copying clinical tables to database")

  # Copy tables to the database
  iwalk(my_cdm, ~ DBI::dbAppendTable(ctn, name = .y, value = .x))

  # ACTIVATE INDEXES ====
  ddl_path <- database_engine %>%
    switch(
      sqlite = NULL,
      postgres = "https://raw.githubusercontent.com/OHDSI/CommonDataModel/master/PostgreSQL/OMOP%20CDM%20postgresql%20pk%20indexes.txt",
      mysql = "https://raw.githubusercontent.com/OHDSI/CommonDataModel/master/Sql%20Server/OMOP%20CDM%20sql%20server%20pk%20indexes.txt"
    )

  download.file(
    ddl_path,
    destfile = file.path(project_path, "ddl/index.txt"))

  qrys <- readr::read_lines(file.path(project_path, "ddl/index.txt")) %>%
    str_subset(";") %>%
    magrittr::extract(2:length(.))

  qry_result <- purrr::map_lgl(.x = qrys, .f = ~ transact(ctn, .x))

  if (all(qry_result)) {
    rlang::inform("Table indexes have been successfully created")
  } else {
    rlang::abort("Problem writing table indexes")
  }

  # ACTIVATE CONSTRAINTS ====
  ddl_path <- database_engine %>%
    switch(
      sqlite = NULL,
      postgres = "https://raw.githubusercontent.com/OHDSI/CommonDataModel/master/PostgreSQL/OMOP%20CDM%20postgresql%20constraints.txt",
      mysql = "https://raw.githubusercontent.com/OHDSI/CommonDataModel/master/Sql%20Server/OMOP%20CDM%20sql%20server%20constraints.txt"
    )

  download.file(
    ddl_path,
    destfile = file.path(project_path, "ddl/constraints.txt"))

  qrys <- readr::read_lines(file.path(project_path, "ddl/constraints.txt")) %>%
    str_subset(";") %>%
    magrittr::extract(2:length(.))

  qry_result <- purrr::map_lgl(.x = qrys, .f = ~ transact(ctn, .x))

  if (all(qry_result)) {
    rlang::inform("Tables constraints have been written successfully")
  } else {
    rlang::abort("Problem writing table constaints")
  }

  return(TRUE)

}

