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
extract_vocab <- function(vocabulary_path) {

  # Identify Files
  csvs <- list.files(vocabulary_path)
  fnames <- str_sub(tolower(csvs), 1, -5)
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

  # Match up the order
  csvs <- csvs[match(names(col_specs), fnames)]

  # Read
  vocab <- map2(
    .x = csvs,
    .y = col_specs,
    .f = ~ read_delim(
      file = .x,
      col_types = .y,
      quote = "",
      delim = "\t",
      progress = FALSE)
  )

  return(vocab)

}
