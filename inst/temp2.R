library(dbplyr)

ctn <- inspectEHR::connect("cc_hic_omop", username = "edward", password = "sucrose-milkman-cogitate")
#vocab <- collect(tbl(ctn, "concept"))

vocab <- read_tsv("~/Downloads/vocabulary_download_v5_{c8e18ce4-8495-4f06-b19e-2442f06fe9e8}_1572008533581/CONCEPT.csv")

vocab <- vocab %>%
  mutate(concept_name = gsub("-", "", tolower(concept_name)))

chk_trm <- "assisted conception used for recent pregnancy"
chk_trm <- "hiv/aids"

# meddic <- gsub("-", "", tolower(meddic))
#
# wrds <- chk_trm %>%
#   str_split(" ") %>%
#   unlist() %>%
#   match(meddic)
#
# meddic[wrds]
#
# match("for", meddic)

match(chk_trm, vocab$concept_name)

poss_match <- agrep("pregnancy", vocab$concept_name)
vocab$concept_name[poss_match]


vocab$concept_name[141201]

vocab %>%
  filter(concept_name == "assisted")

"HIV/AIDS"
