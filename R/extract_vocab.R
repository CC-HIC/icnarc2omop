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
extract_vocab <- function(vocabulary_path, enforce_constraints = TRUE) {

  # Identify Files
  csvs <- list.files(vocabulary_path)
  fnames <- str_sub(tolower(csvs), 1, -5)
  csvs <- csvs[grepl("\\.csv$", csvs)]
  csvs <- file.path(vocabulary_path, csvs)

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

  col_constraints <- list(
    concept = c(
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NULL",
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NULL"),
    concept_ancestor = c(
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NOT NULL"
    ),
    concept_class = c(
      "NOT NULL",
      "NOT NULL",
      "NOT NULL"
    ),
    concept_relationship = c(
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NULL"
    ),
    concept_synonym = c(
      "NOT NULL",
      "NOT NULL",
      "NOT NULL"
    ),
    domain = c(
      "NOT NULL",
      "NOT NULL",
      "NOT NULL"
    ),
    drug_strength = c(
      "NOT NULL",
      "NOT NULL",
      "NULL",
      "NULL",
      "NULL",
      "NULL",
      "NULL",
      "NULL",
      "NULL",
      "NOT NULL",
      "NOT NULL",
      "NULL"
    ),
    relationship = c(
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NOT NULL"
    ),
    vocabulary = c(
      "NOT NULL",
      "NOT NULL",
      "NOT NULL",
      "NULL",
      "NOT NULL"
    )
  )

  # Match up the order
  csvs <- csvs[match(names(col_specs), fnames)]

  # Read in
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

  names(vocab) <- names(col_specs)

  if (enforce_constraints) {
  # Check constraints
    for (i in 1:length(col_constraints)) {
      names(col_constraints[[i]]) <- names(vocab[[i]])
    }

    # Removes fiels that violate constraints
    vocab <- vocab %>%
      imap(
        ~ filter_at(.x,
          .vars =
            vars(names(col_constraints[[.y]][col_constraints[[.y]] == "NOT NULL"])),
            .vars_predicate = all_vars(!is.na(.))))

  }

  return(vocab)

}
