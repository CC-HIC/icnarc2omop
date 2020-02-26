data_spec <- tibble::tribble(
  ~icnarc, ~type,
  "NHSNO", as.character,
  "ADNO", as.integer,
  "ICNNO", as.character,
  "DOB", function(x) lubridate::as_date(x, tz = "GMT"),
  "PCODE", as.character,
  "ETHNIC", as.character,
  "HCM", as.integer,
  "WKG", as.integer,
  "SEX", as.character,
  "DAH", function(x) lubridate::as_date(x, tz = "GMT"),
  "DDH", function(x) lubridate::as_date(x, tz = "GMT"),
  "HDIS", as.character,
  "DAICU", function(x) lubridate::as_date(x, tz = "GMT"),
  "TAICU", hms::as_hms,
  "DDICU", function(x) lubridate::as_date(x, tz = "GMT"),
  "TDICU", hms::as_hms,
  "DIS", as.character,
  "BSDTP", as.logical,
  "DDBSD", function(x) lubridate::as_date(x, tz = "GMT"),
  "TDBSD", hms::as_hms,
  "DBRICU", function(x) lubridate::as_date(x, tz = "GMT"),
  "TBRICU", hms::as_hms,
  "DOD", function(x) lubridate::as_date(x, tz = "GMT"),
  "TOD", hms::as_hms,
  "PLOCA", as.character,
  "RESA", as.character,
  "RESD", as.character,
)

load("./inst/cdm_tables.RData")

usethis::use_data(cdm, data_spec, internal = TRUE, overwrite = TRUE)
