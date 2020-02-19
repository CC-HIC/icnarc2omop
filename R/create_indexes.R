create_indexes <- function(ctn, cdm_version, database_engine, project_path) {

  if (cdm_version == "5.3.1") {

    ddl_path <- database_engine %>%
      switch(
        sqlite = NULL,
        postgres = system.file(
          "dll/5_3_1/PostgreSQL",
          "OMOP CDM postgresql indexes.txt",
          package = "icnarc2omop"),
        mysql = system.file(
          "dll/5_3_1/Sql Server",
          "OMOP CDM sql server indexes.txt",
          package = "icnarc2omop")
      )

  }

  if (cdm_version == "6.0.0") {

    ddl_path <- database_engine %>%
      switch(
        sqlite = NULL,
        postgres = system.file(
          "dll/6_0_0/PostgreSQL",
          "OMOP CDM postgresql indexes.txt",
          package = "icnarc2omop"),
        mysql = system.file(
          "dll/5_3_1/Sql Server",
          "OMOP CDM sql server indexes.txt",
          package = "icnarc2omop")
      )

  }

  qrys <- read_lines(ddl_path) %>%
    str_subset(";") %>%
    extract(2:length(.))

  qry_result <- map_lgl(.x = qrys, .f = ~ transact(ctn, .x))

  if (all(qry_result)) {
    inform("Table indexes have been successfully created")
  } else {
    abort("Problem writing table indexes")
  }

}
