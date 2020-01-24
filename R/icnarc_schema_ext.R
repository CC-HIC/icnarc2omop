# Note, this first part cannot run without the ICNARC schema
# I have not shared as I think this is under copywrite.
library(tidyverse)
library(xml2)
schema <- read_xml("./inst/ICMPDSv3_1.xsd")

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
