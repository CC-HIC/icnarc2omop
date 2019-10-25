#' Create an OMOP database from ICNARC XML
#'
#' This function instantionates an OMOP CDM 6 (OHDSI) database from raw ICNARC XML. In doing so, it creates
#' an extremely spartan instance of OMOP. This behaviour is intentional to short cut the creation of OMOP.
#' After the OMOP database has been created, you have the option to add more data directly from more ICNARC
#' XML in the future, or begin to populate the database directly with data from another source.
#'
#' @param xml_path the path to a folder with ICNARC style XML.
#' @param vocab_path the path to a folder with OMOP style Vocabularies.
#' @param nhs_trust a character string with the full name of the trust.
#' @param source_description path of a comprehensive note about the origin and provenance of the ICNARC XML
#'   for example, what systems in the hospital are used to compile the XML, or is it created by hand.
#'   Should point to a plain text file.
#' @param cdm_version the version of the CDM you are using
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
#'
#' @return TRUE if completed without errors
#' @export
omopify_xml <- function(xml_path,
                        vocab_path,
                        nhs_trust,
                        source_description,
                        cdm_version = "6.0.0",
                        vocabulary_version = "5",
                        database_name = NULL,
                        database_engine = "postgres",
                        host_name = "localhost",
                        port_no = 5432,
                        username = NULL,
                        password = NULL,
                        sqlite_file = NULL) {

  stopifnot(
    all(!is.null(database_name), !is.null(username), !is.null(password)) ||
      !is.null(sqlite_file)
    )

  db_options <- c(
    "sqlite",
    "postgres",
    #"mariadb",
    "mysql",
    "odbc"
    )

  stopifnot(
    any(database_engine %in% db_options)
  )

  rlang::inform("Attempting to connect to database")

  if (is.null(sqlite_file)) {

    db_engine <- database_engine %>%
      switch(
        sqlite <- NULL,
        postgres <- RPostgres::Postgres,
        #mariadb <- RMariaDB::MariaDB,
        mysql <- RMySQL::MySQL,
        odbc <- odbc::odbc
      )

    ctn <- DBI::dbConnect(
      db_engine(),
      host = host_name,
      port = port_no,
      user = username,
      password = password,
      dbname = database_name
      )
  } else {
    ctn <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
  }

  rlang::inform("Connection established")
  rlang::inform("Reading XML")
  # Convert XML to the correct form
  df <- extract_xml(xml_path)

  # Set up tables according to the CDM 6 Schema
  # Capture tables in list
  rlang::inform("Starting CDM build")
  my_cdm <- list()

  # STANDARDISED METADATA
  max_date <- max(df$daicu, na.rm = TRUE)
  my_cdm[["cdm_source"]] <-
    setup_cdm_source(
      nhs_trust,
      source_description,
      max_date,
      cdm_version,
      vocabulary_version
      )
  my_cdm[["metadata"]] <- setup_metadata()

  # STANDARDISED HEALTH SYSTEM
  my_cdm[["location"]] <- setup_location(df, nhs_trust)
  # Must to location history out of sequence
  my_cdm[["care_site"]] <- setup_care_site(df, nhs_trust)
  my_cdm[["provider"]] <- setup_provider()

  # STANDARDISED CLINICAL DATA
  my_cdm[["person"]] <- setup_person(df, my_cdm[["location"]])
  my_cdm[["observation_period"]] <-
    setup_observation_period(my_cdm[["person"]])
  # Interruption to sequence to population location history
  my_cdm[["location_history"]] <-
    setup_location_history(
      df, my_cdm[["observation_period"]], my_cdm[["location"]])
  # Continue
  my_cdm[["visit_occurrence"]] <-
    setup_visit_occurrence(df, my_cdm[["care_site"]], nhs_trust)
  my_cdm[["visit_detail"]] <-
    setup_visit_detail(df, my_cdm[["care_site"]], my_cdm[["visit_occurrence"]])
  my_cdm[["condition_occurrence"]] <- setup_condition_occurrence()
  my_cdm[["drug_exposure"]] <- setup_drug_exposure()
  my_cdm[["procedure_occurrence"]] <- setup_procedure_occurrence()
  my_cdm[["drug_exposure"]] <- setup_drug_exposure()
  my_cdm[["device_exposure"]] <- setup_device_exposure()
  my_cdm[["measurement"]] <- setup_measurement()
  my_cdm[["note"]] <- setup_note()
  my_cdm[["note_nlp"]] <- setup_note_nlp()
  my_cdm[["survey_conduct"]] <- setup_survey_conduct()
  my_cdm[["observation"]] <- setup_observation()
  my_cdm[["specimen"]] <- setup_specimen()
  my_cdm[["fact_relationship"]] <-
    setup_fact_relationship(my_cdm[["care_site"]], nhs_trust)

  # STANDARDISED HEALTH ECONOMICS
  my_cdm[["payer_plan_period"]] <- setup_payer_plan_period()
  my_cdm[["cost"]] <- setup_cost()

  # STANDARDISED DERIVED ELEMENTS

  my_cdm[["drug_era"]] <- setup_drug_era()
  my_cdm[["dose_era"]] <- setup_dose_era()
  my_cdm[["condition_era"]] <- setup_condition_era()

  # RESULTS SCHEMA

  my_cdm[["cohort_tbl"]] <- setup_cohort()
  my_cdm[["cohort_definition_tbl"]] <- setup_cohort_definition()

  rlang::inform("Finished CDM build")
  rlang::inform("Copying clinical tables to database")
  # Copy tables to the database
  iwalk(my_cdm, ~ copy_to(
    ctn, .x, name = .y, overwrite = TRUE, temporary = FALSE)
    )

  # Copy vocabulary to the database
  rlang::inform("Copying vocabulary tables to database")
  my_vocab <- extract_vocab(vocab_path)

  iwalk(my_vocab, ~ copy_to(
    ctn, .x, name = .y, overwrite = TRUE, temporary = FALSE)
  )

  # Activate indexes
  # Activate constaints

  return(TRUE)

}

# STANDARDISED METADATA ====
#' @importFrom readr read_file
setup_cdm_source <- function(
  nhs_trust,
  source_description,
  max_date,
  cdm_version,
  vocabulary_version
  ) {

  text_description <- read_file(source_description)

  all_tables[["cdm_source"]] %>%
    mutate(
      cdm_source_name = "ICARNC Raw Submission XML",
      cdm_source_abbreviation = "ICNARC",
      cdm_holder = paste(nhs_trust, "& UCL"),
      source_description = text_description,
      cdm_etl_reference = "https://github.com/CC-HIC/icnarc2omop",
      source_release_date = max_date,
      cdm_release_date = Sys.Date(),
      cdm_verion = cdm_version,
      vocabulary_version = vocabulary_version
      )

}

setup_metadata <- function() {

  return(all_tables[["metadata"]])

}


# STANDARDISED HEALTH SYSTEM ====

setup_location <- function(input_data, nhs_trust) {

  loc <- input_data %>%
    mutate(vpcode = verify_post_code(.data[["pcode"]])) %>%
    filter(!is.na(.data[["pcode"]]) & .data[["vpcode"]] == TRUE) %>%
    distinct(.data[["pcode"]]) %>%
    rename(zip = .data[["pcode"]]) %>%
    mutate(
      address_1 = as.character(NA),
      address_2 = as.character(NA),
      city = as.character(NA),
      state = as.character(NA),
      county = as.character(NA),
      country = as.character(NA),
      latitude = as.double(NA),
      longitude = as.double(NA)
    ) %>%
    mutate(location_source_value = .data[["zip"]]) %>%
    add_row(location_source_value = .data[["nhs_trust"]])

  site_codes <- input_data %>%
    distinct(.data[["icnno"]]) %>%
    rename(location_source_value = .data[["icnno"]])

  bind_rows(loc, site_codes) %>%
  mutate(location_id = 1:n()) %>%
  select(!!!names(all_tables[["location"]]))

}

#' @importFrom dplyr arrange lead ungroup
setup_location_history <- function(
  input_data, observation_period_table, location_table) {

  ot <- observation_period_table %>%
    select(
      .data[["person_id"]],
      .data[["observation_period_start_date"]],
      .data[["observation_period_end_date"]])

  lt <- location_table %>%
    select(
      .data[["location_id"]],
      .data[["location_source_value"]])

  input_data %>%
    mutate(vpcode = verify_post_code(
      .data[["pcode"]]
      )
    ) %>%
    filter(!is.na(.data[["pcode"]]) & .data[["vpcode"]] == TRUE) %>%
    distinct(.data[["pcode"]], .keep_all = TRUE) %>%
    select(
      .data[["pcode"]],
      .data[["person_id"]],
      .data[["daicu"]]) %>%
    left_join(lt, by = c("pcode" = "location_source_value")) %>%
    left_join(ot, by = "person_id") %>%
    arrange(
      .data[["person_id"]],
      .data[["daicu"]]) %>%
    group_by(
      .data[["person_id"]]) %>%
    mutate(
      n_admissions = n(),
      admission_no = 1:n()
    ) %>%
    filter(
      .data[["n_admissions"]] > 1L) %>%
    mutate(
      start_date = if_else(
        .data[["admission_no"]] == 1L,
        .data[["observation_period_start_date"]],
        .data[["daicu"]]
      ),
      end_date = if_else(
        .data[["admission_no"]] == 1L,
        lead(.data[["daicu"]]),
        .data[["observation_period_end_date"]]
      ),
      relationship_type_concept_id = 46233680L
    ) %>%
    ungroup() %>%
    mutate(location_history_id = 1:n(),
           domain_id = 56L) %>%
    rename(entity_id = .data[["person_id"]]) %>%
    select(!!!names(all_tables[["location_history"]]))

}


#' @importFrom dplyr rename add_row
setup_care_site <- function(input_data, nhs_trust) {

  input_data %>%
    distinct(.data[["icnno"]]) %>%
    rename(care_site_source_value = .data[["icnno"]]) %>%
    add_row(care_site_source_value = nhs_trust) %>%
    bind_rows(all_tables[["care_site"]]) %>%
    mutate(
      care_site_id = 1:n(),
      place_of_service_concept_id = 0L
    ) %>%
    select(!!!names(all_tables[["care_site"]]))

}

setup_provider <- function() {

  return(all_tables[["provider"]])

}

# STANDARDISED CLINICAL DATA ====

#' @importFrom lubridate year month day
setup_person <- function(input_data, location_table) {

  input_data %>%
    group_by(.data[["key"]]) %>%
    arrange(.data[["adno"]]) %>%
    summarise(
      person_id = unique(
        .data[["person_id"]], na.rm = TRUE),
      sex = unique(
        .data[["sex"]], na.rm = TRUE),
      dob = unique(
        .data[["dob"]], na.rm = TRUE),
      dod = select_max(.data[["dod"]]),
      tod = select_max(.data[["tod"]]),
      pcode = select_last(.data[["pcode"]]),
      ethnic = select_mode(.data[["ethnic"]])
    ) %>%
    mutate(
      gender_concept_id = if_else(
        .data[["sex"]] == "F", 8532L, 8507L, missing = as.integer(NA)
      ),
      year_of_birth = as.integer(year(.data[["dob"]])),
      month_of_birth = as.integer(month(.data[["dob"]])),
      day_of_birth = as.integer(day(.data[["dob"]])),
      birth_datetime = as.POSIXct(.data[["dob"]]),
      death_datetime = case_when(
        !is.na(.data[["dod"]]) &
          !is.na(.data[["tod"]]) ~
          as.POSIXct(.data[["dod"]] + seconds(.data[["tod"]])),
        !is.na(.data[["dod"]]) &
          is.na(.data[["tod"]]) ~
          as.POSIXct(.data[["dod"]]),
        TRUE ~ as.POSIXct(NA)
      ),
      race_concept_id = 0L,
      ethnicity_concept_id = 0L,
      location_source_value = .data[["pcode"]], # This is going to get swapped out
      provider_id = 0L,
      care_site_id = 0L,
      person_source_value = .data[["key"]],
      gender_source_value = .data[["sex"]],
      gender_source_concept_id = if_else(
        .data[["sex"]] == "F", 8532L, 8507L, missing = as.integer(NA)
      ),
      race_source_value = .data[["ethnic"]],
      race_source_concept_id = case_when(
        .data[["ethnic"]] == "A" ~ 46286810L,
        .data[["ethnic"]] == "B" ~ 46285824L,
        .data[["ethnic"]] == "C" ~ 46285826L,
        .data[["ethnic"]] == "D" ~ 46285827L,
        .data[["ethnic"]] == "E" ~ 46285828L,
        .data[["ethnic"]] == "F" ~ 46285829L,
        .data[["ethnic"]] == "G" ~ 46285830L,
        .data[["ethnic"]] == "H" ~ 46285831L,
        .data[["ethnic"]] == "J" ~ 46285832L,
        .data[["ethnic"]] == "K" ~ 46285833L,
        .data[["ethnic"]] == "L" ~ 46285835L,
        .data[["ethnic"]] == "M" ~ 46285836L,
        .data[["ethnic"]] == "N" ~ 46286811L,
        .data[["ethnic"]] == "P" ~ 46285837L,
        .data[["ethnic"]] == "R" ~ 46285834L,
        .data[["ethnic"]] == "S" ~ 46285839L,
        .data[["ethnic"]] == "Z" ~ 0L
      ),
      ethnicity_source_value = as.character(NA),
      ethnicity_source_concept_id = 0L
    ) %>%
    left_join(location_table %>%
                select(
                  .data[["location_id"]],
                  .data[["location_source_value"]]),
              by = "location_source_value") %>%
    select(!!!names(all_tables[["person"]]))
}


setup_observation_period <- function(person_table) {

  person_table %>%
    select(.data[["person_id"]], .data[["death_datetime"]]) %>%
    mutate(
      observation_period_id = 1:n(),
      observation_period_start_date = as_date("2014-01-01"),
      observation_period_end_date = if_else(
        !is.na(.data[["death_datetime"]]),
        as_date(.data[["death_datetime"]]) + months(6), Sys.Date()
      ),
      period_type_concept_id = 44814724) %>%
    select(!!!names(all_tables[["observation_period"]]))

}

setup_visit_occurrence <- function(input_data, care_site_table, nhs_trust) {

  vo <- input_data %>%
    select(
      .data[["person_id"]],
      .data[["dah"]],
      .data[["daicu"]],
      .data[["taicu"]],
      .data[["dis"]],
      .data[["ddicu"]],
      .data[["tdicu"]],
      .data[["ddh"]],
      .data[["dod"]],
      .data[["tod"]],
      .data[["icnno"]],
      .data[["resa"]],
      .data[["ploca"]],
      .data[["resd"]]
    ) %>%
    arrange(
      .data[["person_id"]],
      .data[["dah"]],
      .data[["daicu"]]) %>%
    distinct(
      .data[["person_id"]],
      .data[["dah"]], .keep_all = TRUE) %>%
    mutate(
      visit_occurrence_id = 1:n(),
      visit_concept_id = 8717L,
      visit_start_date = .data[["dah"]],
      visit_start_datetime = as.POSIXct(.data[["dah"]]),
      visit_end_date = case_when(
        !is.na(.data[["ddh"]]) ~ .data[["ddh"]],
        !is.na(.data[["dod"]]) ~ .data[["dod"]],
        !is.na(.data[["ddicu"]]) ~ .data[["ddicu"]]
      ),
      visit_end_datetime = as.POSIXct(
        .data[["visit_end_date"]]),
      visit_type_concept_id = 44818518L,
      provider_id = 0L,
      care_site_id = nhs_trust,
      visit_source_value = as.character(NA),
      visit_source_concept_id = 8717L,
      admitted_from_concept_id = case_when(
        .data[["resa"]] == "M" ~ 581476L,
        .data[["resa"]] == "U" ~ 8863L,
        .data[["resa"]] == "H" ~ 38004515L,
        .data[["resa"]] == "O" ~ 42898160L,
        .data[["resa"]] == "R" ~ 581475L,
        .data[["resa"]] == "P" ~ 8546L,
        .data[["resa"]] == "N" ~ 8672L,
        is.na(.data[["resa"]]) ~ 0L
      ),
      admitted_from_source_value = .data[["resa"]],
      discharge_to_concept_id = case_when(
        .data[["resa"]] == "M" ~ 581476L,
        .data[["resa"]] == "U" ~ 8863L,
        .data[["resa"]] == "S" ~ 8920L,
        .data[["resa"]] == "L" ~ 8920L,
        .data[["resa"]] == "H" ~ 38004515L,
        .data[["resa"]] == "O" ~ 42898160L,
        .data[["resa"]] == "R" ~ 581475L,
        .data[["resa"]] == "P" ~ 8546L,
        .data[["resa"]] == "N" ~ 8672L,
        is.na(.data[["resd"]]) ~ 0L
      ),
      discharge_to_source_value = .data[["resd"]]
    )

  vo <- vo %>%
    arrange(
      .data[["person_id"]],
      .data[["visit_start_datetime"]]) %>%
    group_by(
      .data[["person_id"]]) %>%
    mutate(preceding_visit_occurrence_id = lag(
      .data[["visit_occurrence_id"]], 1)) %>%
    select(!!!names(all_tables[["visit_occurrence"]]))

  cs <- care_site_table %>%
    filter(.data[["care_site_source_value"]] == nhs_trust) %>%
    select(.data[["care_site_id"]],
           .data[["care_site_source_value"]])

  vo %>%
    rename(site_code = .data[["care_site_id"]]) %>%
    left_join(
      cs, by = c("site_code" = "care_site_source_value")
    ) %>%
    select(!!!names(all_tables[["visit_occurrence"]]))

}

#' @importFrom lubridate seconds
#' @importFrom dplyr lag
setup_visit_detail <- function(
  input_data,
  care_site_table,
  visit_occurrence_table) {

  input_data %>%
    select(
      .data[["person_id"]],
      .data[["dah"]],
      .data[["daicu"]],
      .data[["taicu"]],
      .data[["dis"]],
      .data[["ddicu"]],
      .data[["tdicu"]],
      .data[["ddh"]],
      .data[["dod"]],
      .data[["tod"]],
      .data[["icnno"]],
      .data[["resa"]],
      .data[["ploca"]],
      .data[["resd"]]
      ) %>%
    mutate(visit_detail_id = 1:n(),
           visit_detail_concept_id = 32037L,
           visit_detail_type_concept_id = 44818518L,
           provider_id = as.integer(0)) %>%
    mutate(
      visit_detail_start_date = .data[["daicu"]],
      visit_detail_start_datetime = as.POSIXct(
        .data[["daicu"]] + seconds(.data[["taicu"]])),
      visit_detail_end_date = case_when(
        !is.na(.data[["daicu"]]) ~ .data[["daicu"]],
        !is.na(.data[["dod"]]) ~ .data[["dod"]]
      ),
      visit_detail_end_datetime = case_when(
        !is.na(.data[["ddicu"]]) & !is.na(.data[["tdicu"]]) ~
          as.POSIXct(.data[["ddicu"]] + seconds(.data[["tdicu"]])),
        !is.na(.data[["ddicu"]]) & is.na(.data[["tdicu"]]) ~
          as.POSIXct(.data[["ddicu"]]),
        is.na(.data[["ddicu"]]) &
          !is.na(.data[["dod"]]) &
          !is.na(.data[["tod"]]) ~ as.POSIXct(
            .data[["dod"]] + seconds(.data[["tod"]])),
        is.na(.data[["ddicu"]]) & !is.na(.data[["dod"]]) &
          is.na(.data[["tod"]]) ~ as.POSIXct(.data[["dod"]]))) %>%
    left_join(
      care_site_table %>%
        select(
          .data[["care_site_source_value"]], .data[["care_site_id"]]),
      by = c("icnno" = "care_site_source_value")
    ) %>%
    mutate(
      visit_detail_source_value = as.character(NA),
      visit_detail_source_concept_id = as.integer(0),
      admitted_from_concept_id = case_when(
        .data[["resa"]] == "M" ~ 581476L,
        .data[["resa"]] == "U" ~ 8863L,
        .data[["resa"]] == "H" ~ 38004515L,
        .data[["resa"]] == "O" ~ 42898160L,
        .data[["resa"]] == "R" ~ 581475L,
        .data[["resa"]] == "P" ~ 8546L,
        .data[["resa"]] == "N" ~ 8672L,
        is.na(.data[["resa"]]) ~ 0L
      ),
      admitted_from_source_value = .data[["resa"]],
      discharge_to_concept_id = case_when(
        .data[["resa"]] == "M" ~ 581476L,
        .data[["resa"]] == "U" ~ 8863L,
        .data[["resa"]] == "S" ~ 8920L,
        .data[["resa"]] == "L" ~ 8920L,
        .data[["resa"]] == "H" ~ 38004515L,
        .data[["resa"]] == "O" ~ 42898160L,
        .data[["resa"]] == "R" ~ 581475L,
        .data[["resa"]] == "P" ~ 8546L,
        .data[["resa"]] == "N" ~ 8672L,
        is.na(resd) ~ 0L
      ),
      discharge_to_source_value = .data[["resd"]]
    ) %>%
    arrange(
      .data[["person_id"]],
      .data[["visit_detail_start_datetime"]]) %>%
    group_by(
      .data[["person_id"]]) %>%
    mutate(preceding_visit_detail_id = lag(
      .data[["visit_detail_id"]], 1)) %>%
    left_join(visit_occurrence_table %>%
                select(
                  .data[["visit_occurrence_id"]],
                  .data[["person_id"]],
                  .data[["visit_start_date"]]),
              by = c("person_id" = "person_id", "dah" = "visit_start_date")) %>%
    mutate(visit_detail_parent_id = as.integer(NA)) %>%
    select(!!!names(all_tables[["visit_detail"]])) %>%
    ungroup()

}


setup_condition_occurrence <- function() {

  return(all_tables[["condition_occurrence"]])

}


setup_drug_exposure <- function() {

  return(all_tables[["drug_exposure"]])

}


setup_procedure_occurrence <- function() {

  return(all_tables[["procedure_occurrence"]])

}

setup_device_exposure <- function() {

  return(all_tables[["device_exposure"]])

}


setup_measurement <- function() {

  return(all_tables[["measurement"]])

}

setup_note <- function() {

  return(all_tables[["note"]])

}

setup_note_nlp <- function() {

  return(all_tables[["note_nlp"]])

}

setup_survey_conduct <- function() {

  return(all_tables[["survey_conduct"]])

}


setup_observation <- function() {

  return(all_tables[["observation"]])

}


setup_specimen <- function() {

  return(all_tables[["specimen"]])

}


#' @importFrom dplyr pull as_tibble
setup_fact_relationship <- function(care_site_table, nhs_trust) {

  trust <- care_site_table %>%
    filter(.data[["care_site_source_value"]] == nhs_trust) %>%
    select(.data[["care_site_id"]]) %>%
    pull()

  sites <- care_site_table %>%
    filter(.data[["care_site_source_value"]] != nhs_trust) %>%
    select(.data[["care_site_id"]]) %>%
    pull()

  trust_sites <- expand.grid(fact_id_1 = trust, fact_id_2 = sites) %>%
    as_tibble() %>%
    mutate(
      domain_concept_id_1 = 57L,
      domain_concept_id_2 = 57L,
      relationship_concept_id = 46233689L
    )

  sites_trust <- expand.grid(fact_id_1 = sites, fact_id_2 = trust) %>%
    as_tibble() %>%
    mutate(
      domain_concept_id_1 = 57L,
      domain_concept_id_2 = 57L,
      relationship_concept_id = 46233688L
    )

  bind_rows(
    trust_sites,
    sites_trust
    ) %>%
    select(!!!names(all_tables[["fact_relationship"]]))

}

# STANDARDISED HEALTH ECONOMICS ====

setup_payer_plan_period <- function() {

  return(all_tables[["payer_plan_period"]])

}

setup_cost <- function() {

  return(all_tables[["cost"]])

}


# STANDARDISED DERIVED ELEMENTS ====

setup_drug_era <- function() {

  return(all_tables[["drug_era"]])

}

setup_dose_era <- function() {

  return(all_tables[["dose_era"]])

}

setup_condition_era <- function() {

  return(all_tables[["condition_era"]])

}

# RESULTS SCHEMA ====

setup_cohort <- function() {

  return(all_tables[["cohort"]])

}

setup_cohort_definition <- function() {

  return(all_tables[["cohort_definition"]])

}


