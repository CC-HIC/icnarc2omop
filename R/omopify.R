#' Create an OMOP database from ICNARC XML
#'
#' This function instantiates an OHDSI CDM (referred to throughout as OMOP for
#' brevity) database from raw ICNARC XML. It can be specfied to output either
#' version 5.3.1 or 6.0.0. In doing so, it creates an extremely spartan instance
#' of OMOP. This behaviour is intentional to short cut the creation of an OMOP
#' database, which can be a daunting process. After the OMOP database has been
#' created, you have the option to add more data directly from additional ICNARC
#' XML in the future, or begin to populate the database directly with data from
#' another source.
#'
#' @param project_path the path to your project folder with the following
#'   populated folders:
#' \itemize{
#'   \item vocab - constains all ATHENA vocabularies
#'   \item meta - constains metadata on the ICNARC ETL process
#'   \item xml - contains raw ICNARC XML in lexicographical order
#' }
#' @param nhs_trust a character string with the full name of the trust.
#' @param cdm_version the version of the CDM you are using.
#' @param vocabulary_version the version of the vocabulary you are using
#' @param database_name the name of the database you are connecting to
#' @param database_engine the database engine, choose from:
#' \itemize{
#'   \item sqlite: SQLite
#'   \item postgres: PostgreSQL
#'   \item mysql: Microsoft SQL Server (or similar varients)
#' }
#' @param host_name host ip address
#' @param port_no port number
#' @param username username to database (*must* have write privaleges)
#' @param sqlite_file a filename if using sqlite
#' @param indexes logical flag if you want to create with indexes
#' @param constraints logical flag if you want to create with relational
#'   constraints
#' @param from_empty  logical flag if you want to create the OMOP database from
#'   stratch. If you want to first write out the tables yourself this can be
#'   set to FALSE
#' @param vocabulary  logical flag if you want to write out vocabularies
#'
#' @importFrom DBI dbConnect dbDisconnect dbListTables
#' @importFrom dplyr collect tbl filter
#' @importFrom glue glue
#' @importFrom magrittr extract extract2 set_names
#' @importFrom odbc odbc
#' @importFrom purrr iwalk imap map_lgl
#' @importFrom readr read_file read_lines
#' @importFrom rlang inform abort
#' @importFrom RMySQL MySQL
#' @importFrom RPostgres Postgres
#' @importFrom rstudioapi askForPassword
#' @importFrom RSQLite SQLite
#' @importFrom stringr str_extract_all str_subset
#'
#' @return TRUE if completed without errors
#' @export
omopify_xml <- function(project_path,
                        nhs_trust,
                        cdm_version = "5.3.1",
                        vocabulary_version = "5",
                        database_name = NULL,
                        database_engine = "postgres",
                        host_name = "localhost",
                        port_no = 5432,
                        username = NULL,
                        from_empty = TRUE,
                        vocabulary = TRUE,
                        indexes = TRUE,
                        constraints = TRUE,
                        sqlite_file = NULL) {
  fstart <- Sys.time()
  # Check db engine is valid
  database_engine <- tolower(database_engine)
  db_options <- c(
    "sqlite",
    "postgres",
    "mysql"
    )

  if (all(!(database_engine %in% db_options))) {
    abort(
      glue("{database_engine} is not a valid choice. Please select from:\n
                 {db_options}")
    )
  }

  # Check initial project folder structure
  project_dirs <- list.dirs(project_path, full.names = FALSE)
  if (!all(c("meta", "xml", "vocab") %in% project_dirs)) {
    print(project_dirs %in% c("meta", "xml", "vocab"))
    abort("Your folder structure is not correct")
  }

  # Add error folder
  if (!dir.exists(file.path(project_path, "errors"))) {
    dir.create(file.path(project_path, "errors"))
  }

  inform("Attempting to connect to database")

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
      password = askForPassword("Please enter your password"),
      dbname = database_name
      )
  } else {
    ctn <- dbConnect(RSQLite::SQLite(), sqlite_file)
  }

  inform("Connection established")

  if (from_empty) {

    # Confirm database is empty
    tbls <- dbListTables(ctn)
    if (length(tbls) != 0) {
      abort("You should be connecting to an empty database. Try again.")
    }

    if (cdm_version == "5.3.1") {

      ddl_path <- database_engine %>%
        switch(
          sqlite = NULL,
          postgres = system.file(
            "dll/5_3_1/PostgreSQL",
            "OMOP CDM postgresql ddl.txt",
            package = "icnarc2omop"),
          mysql = system.file(
            "dll/5_3_1/Sql Server",
            "OMOP CDM sql server ddl.txt",
            package = "icnarc2omop")
        )

    }

    if (cdm_version == "6.0.0") {

      ddl_path <- database_engine %>%
        switch(
          sqlite = NULL,
          postgres = system.file(
            "dll/6_0_0/PostgreSQL",
            "OMOP CDM postgresql ddl.txt",
            package = "icnarc2omop"),
          mysql = system.file(
            "dll/5_3_1/Sql Server",
            "OMOP CDM sql server ddl.txt",
            package = "icnarc2omop")
        )

    }

    # Send create table statements
    qrys <- read_file(ddl_path) %>%
      str_extract_all("(?s)(?<=CREATE TABLE).*?(?=;)") %>%
      extract2(1) %>%
      paste0("CREATE TABLE", ., ";")

    qry_result <- map_lgl(.x = qrys, .f = ~ transact(ctn, .x))

    if (all(qry_result)) {
      inform("Empty tables have been written successfully")
    } else {
      abort("Problem writing out tables to database")
    }

  }

  ## Retrive tables from the database.
  ## We need the datatypes and structures.
  ## Any content can be ignored
  table_names <- sort(dbListTables(ctn))

  vocabulary_names <- c(
    "concept",
    "vocabulary",
    "domain",
    "concept_class",
    "concept_relationship",
    "relationship",
    "concept_synonym",
    "concept_ancestor",
    "drug_strength"
  )

  table_names <- table_names[!(table_names %in% vocabulary_names)]

  # Collect *NON vocabulary* items
  my_cdm <- imap(
    .x = table_names,
    .f = ~ collect(tbl(ctn, .x))) %>%
    set_names(table_names)

  attr(my_cdm, "version") <- cdm_version

  inform("Reading and converting XML")

  # Convert XML to the correct form
  df <- extract_xml(file.path(project_path, "xml"))
  ## Remove patients still inside the ICU at time of processing
  df <- filter(df, .data[["dis"]] != "E")

  inform("ICNARC XML parsed successfully")

  # Set up tables according to the CDM 6 Schema
  # Capture tables in list
  inform("Starting CDM build")

  # VOCABULARIES ====

  if (vocabulary) {

    inform("Reading in vocabularies")
    my_vocab <- extract_vocab(file.path(project_path, "vocab"))
    inform("Writing vocabularies to database. Go grab a coffee...")
    iwalk(my_vocab, ~ write_notify(conn = ctn, name = .y, value = .x))
    inform("Vocabularies copied to database")
    inform("Organising chickpeas")
    rm(my_vocab)

  }

  inform("Munching ICNARC data to OMOP format")
  # STANDARDISED METADATA
  max_date <- max(df$daicu, na.rm = TRUE)

  my_cdm[["cdm_source"]] <-
    setup_cdm_source(
      my_cdm,
      nhs_trust,
      project_path,
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
  # Note the change in code style here to accomodate death/person tables
  my_cdm <- setup_person_death(my_cdm, df, project_path)
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
  inform("Blending colours")
  my_cdm[["drug_era"]] <- setup_drug_era(my_cdm)
  my_cdm[["dose_era"]] <- setup_dose_era(my_cdm)
  my_cdm[["condition_era"]] <- setup_condition_era(my_cdm)

  # RESULTS SCHEMA

  my_cdm[["cohort_tbl"]] <- setup_cohort(my_cdm)
  my_cdm[["cohort_definition_tbl"]] <- setup_cohort_definition(my_cdm)

  inform("Finished CDM build")
  inform("Copying clinical tables to database")

  # Copy tables to the database
  iwalk(my_cdm, ~ write_notify(conn = ctn, name = .y, value = .x))

  # ACTIVATE INDEXES ====
  if (indexes) {
    create_indexes(ctn, database_engine, project_path)
  }

  # ACTIVATE CONSTRAINTS ====
  if (constraints) {
    activate_constraints(ctn, database_engine, project_path)
  }

  fend <- Sys.time()
  dur <- fend - fstart
  inform(
    glue("Congratulations, your OMOP setup completed in {dur}")
  )
  dbDisconnect(ctn)
  return(TRUE)
}

