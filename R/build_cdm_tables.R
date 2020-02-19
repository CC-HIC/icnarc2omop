# STANDARDISED METADATA ====

#' @importFrom dplyr mutate
#' @importFrom readr read_file
setup_cdm_source <- function(
  inmem_cdm,
  nhs_trust,
  project_path,
  max_date,
  cdm_version,
  vocabulary_version
) {

  text_description <- read_file(file.path(project_path, "meta/src_desc.txt"))

  inmem_cdm[["cdm_source"]] %>%
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

#' @importFrom rlang inform
setup_metadata <- function(inmem_cdm) {

  inform("Metadata table is currently not filled")
  return(inmem_cdm[["metadata"]])

}


# STANDARDISED HEALTH SYSTEM ====

#' @importFrom dplyr mutate select filter distinct rename add_row n
#' @importFrom rlang .data !!!
setup_location <- function(inmem_cdm, input_data, nhs_trust) {

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
    add_row(location_source_value = nhs_trust)

  site_codes <- input_data %>%
    distinct(.data[["icnno"]]) %>%
    rename(location_source_value = .data[["icnno"]])

  bind_rows(loc, site_codes) %>%
    mutate(location_id = 1:n()) %>%
    select(!!!names(inmem_cdm[["location"]]))

}

#' @importFrom dplyr mutate select filter distinct rename add_row n arrange
#'   left_join group_by ungroup lead
#' @importFrom rlang .data !!!
setup_location_history <- function(inmem_cdm, input_data) {

  ot <- inmem_cdm[["observation_period"]] %>%
    select(
      .data[["person_id"]],
      .data[["observation_period_start_date"]],
      .data[["observation_period_end_date"]])

  lt <- inmem_cdm[["location"]] %>%
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
    select(!!!names(inmem_cdm[["location_history"]]))

}


#' @importFrom dplyr distinct rename add_row mutate select left_join
#' @importFrom rlang !!! .data
setup_care_site <- function(inmem_cdm, input_data, nhs_trust) {

  # Enter the trust
  trust <- tibble(care_site_source_value = nhs_trust)

  # Enter the ICUs
  sites <- input_data %>%
    distinct(.data[["icnno"]]) %>%
    rename(care_site_source_value = .data[["icnno"]])

  # Shape into correct format
  bind_rows(trust, sites) %>%
    left_join(inmem_cdm[["location"]] %>%
                  select(.data[["location_id"]],
                         .data[["location_source_value"]]),
      by = c("care_site_source_value" = "location_source_value")) %>%
  bind_rows(inmem_cdm[["care_site"]] %>%
              # Corrects for a bug with int64
              select(-.data[["location_id"]])) %>%
    mutate(
      care_site_id = 1:n(),
      place_of_service_concept_id = 0L
    ) %>%
    select(!!!names(inmem_cdm[["care_site"]]))

}

#' @importFrom rlang inform
setup_provider <- function(inmem_cdm) {

  inform("Providor table is currently not filled")
  return(inmem_cdm[["provider"]])

}

# STANDARDISED CLINICAL DATA ====

#' Setup Person Table
#'
#' @param inmem_cdm
#' @param input_data
#' @param project_path
#'
#' @importFrom lubridate year month day seconds
#' @importFrom dplyr group_by arrange summarise mutate case_when filter_at vars
#'   any_vars if_else left_join
#' @importFrom rlang !!! .data
#' @importFrom readr write_csv
setup_person <- function(inmem_cdm, input_data, project_path) {

  error_key <- input_data %>%
    group_by(.data[["key"]]) %>%
    arrange(.data[["adno"]]) %>%
    summarise(
      sex = length(unique(
        .data[["sex"]], na.rm = TRUE)),
      dob = length(unique(
        .data[["dob"]], na.rm = TRUE)),
      dod = length(unique(.data[["dod"]], na.rm = TRUE)),
      tod = length(unique(.data[["tod"]], na.rm = TRUE))
    ) %>%
    filter_at(vars(sex:tod), any_vars(. > 1))

  if (nrow(error_key) != 0) {
  write_csv(error_key, path = file.path(project_path, "errors/person_errors.csv"))
  }

  input_data %>%
    group_by(.data[["key"]]) %>%
    arrange(.data[["adno"]]) %>%
    summarise(
      person_id = unique(
        .data[["person_id"]], na.rm = TRUE),
      sex = select_last(.data[["sex"]]),
      dob = select_last(.data[["dob"]]),
      dod = select_max(.data[["dod"]]),
      tod = select_max(.data[["tod"]]),
      ddbsd = select_max(.data[["ddbsd"]]),
      tdbsd = select_max(.data[["tdbsd"]]),
      pcode = select_last(.data[["pcode"]]),
      ethnic = select_mode(.data[["ethnic"]])
    ) %>%
    mutate(
      gender_concept_id = if_else(
        .data[["sex"]] == "F", 8532L, 8507L, 0L
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
        !is.na(.data[["ddbsd"]]) &
          !is.na(.data[["tdbsd"]]) ~
          as.POSIXct(.data[["ddbsd"]] + seconds(.data[["tdbsd"]])),
        TRUE ~ as.POSIXct(NA)
      ),
      race_concept_id = 0L,
      ethnicity_concept_id = 0L,
      location_source_value = .data[["pcode"]],
      # Providor is left as NA, these carry no meaning for us.
      provider_id = as.integer(NA),
      # Care site is hard coded to be 1L at this point.
      # This is synonymous with the Trust. Regardless of specific
      # site, which will be referenced in the visit_detail.
      care_site_id = 1L,
      person_source_value = .data[["key"]],
      gender_source_value = .data[["sex"]],
      gender_source_concept_id = if_else(
        .data[["sex"]] == "F", 8532L, 8507L, missing = 0L
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
        .data[["ethnic"]] == "Z" ~ 0L,
        TRUE ~ 0L
      ),
      ethnicity_source_value = as.character(NA),
      ethnicity_source_concept_id = 0L
    ) %>%
    left_join(inmem_cdm[["location"]] %>%
                select(
                  .data[["location_id"]],
                  .data[["location_source_value"]]),
              by = "location_source_value") %>%
    select(!!!names(inmem_cdm[["person"]]))
}


#' Setup Observation Period Table
#'
#' The observation period spans from the beggining of the study period (i.e. 1st
#' Jan 2014) to either today, or 60 days post death.
#'
#' @param inmem_cdm
#'
#' @importFrom lubridate as_date days
#' @importFrom dplyr select mutate if_else
#' @importFrom rlang !!! .data
setup_observation_period <- function(inmem_cdm) {

  inmem_cdm[["person"]] %>%
    select(.data[["person_id"]], .data[["death_datetime"]]) %>%
    mutate(
      observation_period_id = 1:n(),
      observation_period_start_date = as_date("2014-01-01"),
      observation_period_end_date = if_else(
        !is.na(.data[["death_datetime"]]),
        as_date(.data[["death_datetime"]]) + days(60), Sys.Date()
      ),
      period_type_concept_id = 44814724L) %>%
    select(!!!names(inmem_cdm[["observation_period"]]))

}

#' @importFrom dplyr select filter arrange distinct mutate case_when if_else
#'   group_by left_join lag
#' @importFrom rlang !!! .data
setup_visit_occurrence <- function(inmem_cdm, input_data, nhs_trust) {

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
      .data[["ddbsd"]],
      .data[["tdbsd"]],
      .data[["dbricu"]],
      .data[["tbricu"]],
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
        !is.na(.data[["dbricu"]]) ~ .data[["dbricu"]],
        !is.na(.data[["ddbsd"]]) ~ .data[["ddbsd"]],
        !is.na(.data[["ddicu"]]) ~ .data[["ddicu"]]
      ),
      visit_end_datetime = as.POSIXct(
        .data[["visit_end_date"]]),
      visit_type_concept_id = 44818518L,
      # Providor ID not in use
      provider_id = as.integer(NA),
      # Care site ID at hospital admission level always = 1L
      care_site_id = 1L,
      visit_source_value = as.character(NA),
      visit_source_concept_id = 8717L,
      admitted_from_concept_id = case_when(
        .data[["resa"]] == "M" ~ 8536L,
        .data[["resa"]] == "U" ~ 8863L,
        .data[["resa"]] == "H" ~ 8717L,
        .data[["resa"]] == "O" ~ 8844L,
        .data[["resa"]] == "R" ~ 8940L,
        .data[["resa"]] == "P" ~ 8546L,
        .data[["resa"]] == "N" ~ 8672L,
        is.na(.data[["resa"]]) ~ 0L
      ),
      admitted_from_source_value = .data[["resa"]],
      discharge_to_concept_id = case_when(
        .data[["resd"]] == "M" ~ 8536L,
        .data[["resd"]] == "U" ~ 8863L,
        .data[["resd"]] == "S" ~ 8920L,
        .data[["resd"]] == "L" ~ 8920L,
        .data[["resd"]] == "H" ~ 8717L,
        .data[["resd"]] == "O" ~ 8844L,
        .data[["resd"]] == "R" ~ 8940L,
        .data[["resd"]] == "P" ~ 8546L,
        .data[["resd"]] == "N" ~ 8672L,
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
    mutate(preceding_visit_occurrence_id = dplyr::lag(
      .data[["visit_occurrence_id"]], 1)) %>%
    select(!!!names(inmem_cdm[["visit_occurrence"]]))

  # Care site for everyone at the occurence level is trust
  cs <- inmem_cdm[["care_site"]] %>%
    filter(.data[["care_site_source_value"]] == nhs_trust) %>%
    select(.data[["care_site_id"]],
           .data[["care_site_source_value"]])

  vo %>%
    left_join(cs, by = "care_site_id") %>%
    select(!!!names(inmem_cdm[["visit_occurrence"]]))

}

#' @importFrom lubridate seconds
#' @importFrom dplyr select filter arrange distinct mutate case_when if_else
#'   group_by left_join lag n
#' @importFrom rlang !!! .data
setup_visit_detail <- function(inmem_cdm, input_data) {

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
      .data[["ddbsd"]],
      .data[["tdbsd"]],
      .data[["dbricu"]],
      .data[["tbricu"]],
      .data[["icnno"]],
      .data[["resa"]],
      .data[["ploca"]],
      .data[["resd"]]
    ) %>%
    mutate(visit_detail_id = 1:n(),
           visit_detail_concept_id = 32037L,
           visit_detail_type_concept_id = 44818518L,
           provider_id = 0L) %>%
    mutate(
      visit_detail_start_date = .data[["daicu"]],
      visit_detail_start_datetime = as.POSIXct(
        .data[["daicu"]] + seconds(.data[["taicu"]])),
      visit_detail_end_date = case_when(
        .data[["dis"]] == "A" ~ .data[["daicu"]],
        .data[["dis"]] == "D" &
          !is.na(.data[["dod"]]) ~ .data[["dod"]],
        .data[["dis"]] == "D" &
          !is.na(.data[["dbricu"]]) ~ .data[["dbricu"]],
        .data[["dis"]] == "D" &
          !is.na(.data[["ddbsd"]]) ~ .data[["ddbsd"]],
      ),
      visit_detail_end_datetime = case_when(
        .data[["dis"]] == "A" ~
          as.POSIXct(.data[["ddicu"]] + seconds(.data[["tdicu"]])),
        .data[["dis"]] == "D" &
          !is.na(.data[["dod"]]) ~
          as.POSIXct(.data[["dod"]] + seconds(.data[["tod"]])),
        .data[["dis"]] == "D" &
          !is.na(.data[["dbricu"]]) ~
          as.POSIXct(.data[["dbricu"]] + seconds(.data[["tbricu"]])),
        .data[["dis"]] == "D" &
          !is.na(.data[["ddbsd"]]) ~
          as.POSIXct(.data[["ddbsd"]] + seconds(.data[["tdbsd"]])))) %>%
    left_join(
      inmem_cdm[["care_site"]] %>%
        select(
          .data[["care_site_source_value"]], .data[["care_site_id"]]),
      by = c("icnno" = "care_site_source_value")
    ) %>%
    mutate(
      visit_detail_source_value = as.character(NA),
      visit_detail_source_concept_id = 0L,
      admitted_from_concept_id = case_when(
        .data[["resa"]] == "M" ~ 8536L,
        .data[["resa"]] == "U" ~ 8863L,
        .data[["resa"]] == "H" ~ 8717L,
        .data[["resa"]] == "O" ~ 8844L,
        .data[["resa"]] == "R" ~ 8940L,
        .data[["resa"]] == "P" ~ 8546L,
        .data[["resa"]] == "N" ~ 8672L,
        is.na(.data[["resa"]]) ~ 0L
      ),
      admitted_from_source_value = .data[["resa"]],
      discharge_to_concept_id = case_when(
        .data[["resd"]] == "M" ~ 8536L,
        .data[["resd"]] == "U" ~ 8863L,
        .data[["resd"]] == "S" ~ 8920L,
        .data[["resd"]] == "L" ~ 8920L,
        .data[["resd"]] == "H" ~ 8717L,
        .data[["resd"]] == "O" ~ 8844L,
        .data[["resd"]] == "R" ~ 8940L,
        .data[["resd"]] == "P" ~ 8546L,
        .data[["resd"]] == "N" ~ 8672L,
        is.na(.data[["resd"]]) ~ 0L
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
    left_join(inmem_cdm[["visit_occurrence"]] %>%
                select(
                  .data[["visit_occurrence_id"]],
                  .data[["person_id"]],
                  .data[["visit_start_date"]]),
              by = c("person_id" = "person_id", "dah" = "visit_start_date")) %>%
    mutate(visit_detail_parent_id = as.integer(NA)) %>%
    select(!!!names(inmem_cdm[["visit_detail"]])) %>%
    ungroup()

}


setup_condition_occurrence <- function(inmem_cdm) {

  return(inmem_cdm[["condition_occurrence"]])

}


setup_drug_exposure <- function(inmem_cdm) {

  return(inmem_cdm[["drug_exposure"]])

}


setup_procedure_occurrence <- function(inmem_cdm) {

  return(inmem_cdm[["procedure_occurrence"]])

}

setup_device_exposure <- function(inmem_cdm) {

  return(inmem_cdm[["device_exposure"]])

}


setup_measurement <- function(inmem_cdm) {

  return(inmem_cdm[["measurement"]])

}

setup_note <- function(inmem_cdm) {

  return(inmem_cdm[["note"]])

}

setup_note_nlp <- function(inmem_cdm) {

  return(inmem_cdm[["note_nlp"]])

}

setup_survey_conduct <- function(inmem_cdm) {

  return(inmem_cdm[["survey_conduct"]])

}


setup_observation <- function(inmem_cdm) {

  return(inmem_cdm[["observation"]])

}


setup_specimen <- function(inmem_cdm) {

  return(inmem_cdm[["specimen"]])

}


#' @importFrom dplyr filter select mutate pull as_tibble bind_rows
#' @importFrom rlang !!! .data
setup_fact_relationship <- function(inmem_cdm, nhs_trust) {

  trust <- inmem_cdm[["care_site"]] %>%
    filter(.data[["care_site_source_value"]] == nhs_trust) %>%
    select(.data[["care_site_id"]]) %>%
    pull()

  sites <- inmem_cdm[["care_site"]] %>%
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
    select(!!!names(inmem_cdm[["fact_relationship"]]))

}

# STANDARDISED HEALTH ECONOMICS ====

setup_payer_plan_period <- function(inmem_cdm) {

  return(inmem_cdm[["payer_plan_period"]])

}

setup_cost <- function(inmem_cdm) {

  return(inmem_cdm[["cost"]])

}


# STANDARDISED DERIVED ELEMENTS ====

setup_drug_era <- function(inmem_cdm) {

  return(inmem_cdm[["drug_era"]])

}

setup_dose_era <- function(inmem_cdm) {

  return(inmem_cdm[["dose_era"]])

}

setup_condition_era <- function(inmem_cdm) {

  return(inmem_cdm[["condition_era"]])

}

# RESULTS SCHEMA ====

setup_cohort <- function(inmem_cdm) {

  return(inmem_cdm[["cohort"]])

}

setup_cohort_definition <- function(inmem_cdm) {

  return(inmem_cdm[["cohort_definition"]])

}


