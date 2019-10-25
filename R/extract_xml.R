#' Extract data from ICNARC xml
#'
#'
#' @param xml_path the path to a folder with ICNARC style XML. This folder
#'   should contain NOTHING but XML. The XML should be labelled in
#'   lexicographical order (i.e. the order you want the files to be read in).
#'   For example, my_xml01.xml, my_xml02.xml and so on. Please do not rely on
#'   dates in the names parse files in the correct order.
#'
#' @return a data frame with ICNARC data
#' @export
#'
#' @importFrom xml2 read_xml xml_find_all xml_name xml_children xml_length
#'   xml_text
#' @importFrom lubridate as_date
#' @importFrom hms as_hms hms
#' @importFrom tidyr spread
#' @importFrom stringr str_pad
#' @importFrom dplyr select filter group_by summarise tibble bind_rows if_else
#'   vars mutate one_of distinct case_when n left_join
#' @importFrom purrr map pmap map_dfr imap_dfc
#' @importFrom magrittr %>%
#' @importFrom rlang .data !! !!!
extract_xml <- function(xml_path) {

  # Identify Files
  xmls <- list.files(xml_path)
  xmls <- xmls[grepl("\\.xml$", xmls)]
  xmls <- paste0(xml_path, xmls)

  # Read in as XML
  admissions <- map(xmls, ~ read_xml(.x)) %>%
    map(~ xml_find_all(.x, xpath = "//d1:ADMISSION"))

  # Extract the node names
  node_names <- admissions %>%
    map(~ xml_name(xml_children(.x)))

  # Extract contents
  contents <- admissions %>%
    map(~ trimws(xml_text(xml_children(.x))))

  # Calculate future table dimentions
  id_list <- admissions %>%
    map(~ rep(seq_len(.x), xml_length(.x)))

  # Convert to data frame and merge
  df <- pmap(
    list(
      id_list,
      node_names,
      contents
    ),
    ~ tibble(
      id = ..1,
      node_names = ..2,
      contents = ..3)
  ) %>%
    map_dfr(
      ~ mutate(
        .data = .x,
        contents =
          if_else(
            .data[["contents"]] == "" |
              is.na(.data[["contents"]]) |
              .data[["contents"]] == "UNKNOWN",
            as.character(NA),
            .data[["contents"]]
          )
        ) %>%
        spread(node_names, contents)
    ) %>%
    select(one_of(data_spec$icnarc))

  # Transform from chr to the correct data type
  df <- imap_dfc(df, transform_data_type, data_spec)
  names(df) <- tolower(names(df))

  # Create patient ID
  df <- df %>%
    mutate(
      vnhs = verify_nhs(.data[["nhsno"]])
    ) %>%
    mutate(key = case_when(
      !is.na(.data[["nhsno"]]) & .data[["vnhs"]] == TRUE ~ .data[["nhsno"]],
      !is.na(.data[["dob"]]) &
        !is.na(.data[["pcode"]]) &
        !is.na(.data[["sex"]]) ~
        paste(.data[["dob"]], .data[["pcode"]], .data[["sex"]], sep = "|"),
      TRUE ~ as.character(NA))) %>%
    select(-.data[["vnhs"]])

  # Add an ID to those who cannot be identified. All linking potential is now
  # lost
  df <- df %>%
    mutate(
      nolink = paste("NOLINK",
                     str_pad(1:nrow(df),
                             pad = "0",
                             width = 6),
                     sep = "-"),
      key = if_else(
        is.na(.data[["key"]]), .data[["nolink"]], .data[["key"]])) %>%
    select(-.data[["nolink"]])

  key_id <- df %>%
    select(.data[["key"]]) %>%
    distinct() %>%
    mutate(person_id = 1:n())

  df <- left_join(df, key_id, by = "key")

  return(df)
}


#' Transform Data Type
#'
#' Data stored in the XML are inherently character strings. This function
#' transforms the raw data into the correct type, according to the ICNARC data
#' specification.
#'
#' @param input_data vector of data to be transformed
#' @param input_name name for vector to be found in lookup
#' @param lookup a lookup table with conversion functions
#'
#' @return a vector of the datatype specified in lookup
#' @export
transform_data_type <- function(input_data, input_name, lookup) {
  conversion_function <-
    lookup[lookup$icnarc == input_name, 2, drop = TRUE][[1]]
  conversion_function(input_data)
}
