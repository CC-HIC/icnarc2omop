#' Extract OMOP Vocabularies from Source CSV Files
#'
#' Takes a single argument \code{vocabulary_path} which is the local path
#' to the OMOP vocabularies in CSV format. These are an essential component to
#' create a new OMOP database
#'
#' @param vocabulary_path character string containing the path to the folder
#'   containing OMOP vocabularies as CSV.
#'
#' @importFrom readr read_tsv
#' @importFrom purrr map
#'
#' @return a list of tables containing the vocabularies
#' @export
extract_vocab <- function(inmem_cdm, vocabulary_path) {

  # parse_vals <- map(inmem_cdm,
  #     ~ map(.x, ~ class(.x)[1]) %>%
  #       unlist() %>%
  #       as.character()) %>%
  #   map(~ case_when(
  #     .x == "integer" | .x == "integer64" ~ "i",
  #     .x == "character" ~ "c",
  #     .x == "numeric" ~ "d",
  #     .x == "Date" ~ "D",
  #     .x == "POSIXct" ~ "T",
  #     TRUE ~ "?") %>%
  #       paste0(collapse = ""))

  # Identify Files
  csvs <- list.files(vocabulary_path)
  csvs <- csvs[grepl("\\.csv$", csvs)]
  csvs <- paste0(vocabulary_path, csvs)

  ## This was frustratingly difficult to do programatically, due to the
  ## way in which you specify column types in readr. I give up.

  col_specs <- list(
    concept = cols(
      col_integer(),
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_date(format = "%Y%m%d"),
      col_date(format = "%Y%m%d"),
      col_character()),
    concept_ancestor = cols(
      col_integer(),
      col_integer(),
      col_integer(),
      col_integer()
    ),
    concept_class = cols(
      col_character(),
      col_character(),
      col_integer()
    ),
    concept_relationship = cols(
      col_integer(),
      col_integer(),
      col_character(),
      col_date(format = "%Y%m%d"),
      col_date(format = "%Y%m%d"),
      col_character()
    ),
    concept_synonym = cols(
      col_integer(),
      col_character(),
      col_integer()
    ),
    domain = cols(
      col_character(),
      col_character(),
      col_integer()
    ),
    drug_strength = cols(
      col_integer(),
      col_integer(),
      col_double(),
      col_integer(),
      col_double(),
      col_integer(),
      col_double(),
      col_integer(),
      col_integer(),
      col_date(format = "%Y%m%d"),
      col_date(format = "%Y%m%d"),
      col_character()
    ),
    relationship = cols(
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_integer()
    ),
    vocabulary = cols(
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_integer()
    )
  )

  df <- read_delim(
    file = "~/_data/test.csv",
    col_types = cols(
      col_integer(), col_character(), col_character(),
      col_character(), col_character(), col_character(),
      col_character(), col_date(format = "%Y%m%d"), col_date(format = "%Y%m%d"),
      col_character()),
    escape_double = TRUE,
    delim = "\t"
  )

  df


  # Read
  pmap(
    list(csvs, lnames, , ~ read_tsv(.x))

}

# parse_vals <- map(collected_tables,
#                   ~ map(.x, ~ class(.x)[1]) %>%
#                     unlist() %>%
#                     as.character()) %>%
#   map(~ case_when(
#     .x == "integer" | .x == "integer64" ~ ,
#     .x == "character" ~ col_character,
#     .x == "numeric" ~ col_double(),
#     .x == "Date" ~ col_date(format = "%Y%m%d"),
#     TRUE ~ col_guess()))

# Identify Files
csvs <- list.files("~/_data/omop_vocab/")
csvs <- csvs[grepl("\\.csv$", csvs)]

lnames <- gsub(pattern = ".csv", replacement = "", x = tolower(csvs))

csvs <- csvs[match(names(col_specs), lnames)]
csvs <- paste0("~/_data/omop_vocab/", csvs)

# Read
my_vocab <- map2(
  .x = csvs, .y = col_specs, .f = ~ read_tsv(file = .x, col_types = .y))

names(my_vocab) <- names(col_specs)

problems(my_vocab[["concept"]])

my_vocab[["concept"]][435512,"concept_name"]

