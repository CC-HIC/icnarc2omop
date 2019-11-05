library(tidyverse)
library(xml2)
library(pdftools)
library(stringdist)

schema <- read_xml("./inst/ICMPDSv3_1.xsd")
schema


node_names <- xml_find_all(schema, xpath = "//xs:all") %>%
  map_dfc(~ xml_attr(xml_children(.x), attr = "name")) %>%
  rename(icnarc_names = V1)

data_types <- xml_find_all(schema, xpath = "//xs:all") %>%
  map_dfc(~ xml_attr(xml_children(.x), attr = "type")) %>%
  rename(validation = V1)

ic <- bind_cols(node_names, data_types)

ic2 <- ic %>%
  mutate(
    xmll =
      map(validation,
        ~ xml_find_all(
          schema,
          xpath = paste0(
            "//xs:simpleType[@name='", .x, "']")
        )
      ) %>%
      map_int(
        ~ xml_length(.x)
      )
  ) %>%
  mutate(restriction =
    map2(.x = validation, .y = xmll, function(x, y) {
      if (y != 1) {
        return(c("00:00:00", "23:59:59"))
      } else {
        xml_find_all(
          schema,
          xpath = paste0(
            "//xs:simpleType[@name='", x, "']")) %>%
          xml_child() %>%
            xml_children() %>%
            xml_attr(attr = "value")

      }
    }
  )
  )

fls <- pdf_text("./inst/fls.pdf") %>%
  read_lines() %>%
  trimws() %>%
  str_replace_all("([ ][ ])+", "")


fls[max(which(
  grepl(
    paste0(
      "(Field name:)[ ]*(", "ACON", ")"
    ), fls
  )
))+1]


ic2 <- ic2 %>%
  select(-xmll) %>%
  mutate(
    full_name =
      map_chr(
        icnarc_names, function(x) {
          fls[max(
            which(
              grepl(
                paste0(
                  "(Field name:)[ ]*(", x, ")"
                  ), fls
                )
            )
          )+1]
          })
  ) %>%
  mutate(
    full_name =
      gsub("Field:[ ]*", replacement = "", x = full_name)
  )

ic2 <- ic2 %>%
  unnest(col = "restriction") %>%
  mutate(full_name = gsub("-", "", tolower(full_name)))

ic2

fls[grepl(pattern = "options", fls)]

fls <- fls[229:length(fls)]

fls_df <- tibble(txt = fls)

fls_df$new <- as.character(NA)
counter <- 1L
for(i in 1:nrow(fls_df)) {
  if (fls_df$txt[i] == lu$txt[counter]) {
    fls_df[i, 2] <- lu$txt[counter]
    counter <- counter + 1L
  }
}

fls_df %>%
  mutate(new = zoo::na.locf(new)) %>%
  mutate(
    label = trimws(gsub(
      "(Field name:)(.*)(\\(technical specification\\))", "\\2", new
    ))
  ) %>%
  group_by(label) %>%
  filter(grepl("\u0001options:", txt))


