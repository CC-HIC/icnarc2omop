activate_constraints <- function(ctn, database_engine, project_path) {

  ddl_path <- database_engine %>%
    switch(
      sqlite = NULL,
      postgres = "https://raw.githubusercontent.com/OHDSI/CommonDataModel/master/PostgreSQL/OMOP%20CDM%20postgresql%20constraints.txt",
      mysql = "https://raw.githubusercontent.com/OHDSI/CommonDataModel/master/Sql%20Server/OMOP%20CDM%20sql%20server%20constraints.txt"
    )

  download.file(
    ddl_path,
    destfile = file.path(project_path, "ddl/constraints.txt"),
    quiet = TRUE)

  qrys <- read_lines(file.path(project_path, "ddl/constraints.txt")) %>%
    str_subset(";") %>%
    extract(2:length(.))

  qry_result <- map_lgl(.x = qrys, .f = ~ transact(ctn, .x))

  if (all(qry_result)) {
    inform("Tables constraints have been written successfully")
  } else {
    abort("Problem writing table constaints")
  }

}
