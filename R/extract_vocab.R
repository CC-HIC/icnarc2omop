extract_vocab <- function(vocabulary_path) {
  
  # Identify Files
  csvs <- list.files(vocabulary_path)
  csvs <- csvs[grepl("\\.csv$", csvs)]
  csvs <- paste0(vocabulary_path, csvs)
  
  # Read in
  map(csvs, ~ read_csv(.x))

}