check_constraints <- function(vocabulary, path) {
  # Removes fiels that violate constraints
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

  # The following will filter out violations
  # vocab <- vocab %>%
  #   imap(
  #     ~ filter_at(
  #       .x,
  #       .vars = names(.x)[col_constraints[[.y]] == "NOT NULL"],
  #       .vars_predicate = all_vars(!is.na(.))))

  vocab2 <- vocab %>%
    imap(
      ~ filter_at(
        .x,
        .vars = names(.x)[col_constraints[[.y]] == "NOT NULL"],
        .vars_predicate = any_vars(is.na(.))))

  purrr::iwalk(vocab2, .f = ~ readr::write_csv(.x, path = paste0(
    path, .y, ".csv"
  )))

}
