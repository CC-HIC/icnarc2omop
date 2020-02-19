# icnarc2omop

The role of this package is to help convert the most elementary components of the ICNARC XML to OMOP CDM version 6. This facilitates the set up of an OMOP style CDM, acting as a base from which users can then populate with data from their local EHR. This was a somewhat non-trivial task, since the ICNARC specification is episode centric, and the OMOP CDM is patient centric. This is necessitated a number of opinionated design decisions, especially in places where inforamtion of a person's hospital journey is lacking. These decisions may not work universally for all study designs.

## Prerequisites

- A running target empty database
- R (R is mandatory, R Studio is useful)
- The ICNARC xmls you wish to convert
- A text file describing the ETL process that generated the ICNARC files (for data provenance)
- A download of the vocabuaries you would like to use from (Athena)[http://athena.ohdsi.org/]
- A PC with at least 8Gb of memory (moving the OMOP vocabularies through memory is the main bottleneck). If this is a problem, please let me know and I will write a chunking function to mitigate this issue.

## Getting Started

Before using this package, you will need to:

1. Set up a new empty database
2. Create a project folder and populate it with the necessary files. They *MUST* follow this convention *EXACTLY*:

```
|--xml/
    |--icnarc01.xml
    |--icnarc02.xml
|--meta/
    |--src_desc.txt
    |--provenance_info.txt
|--vocab/
    |--CONCEPT_ANCESTOR.csv
    |--CONCEPT.csv
    |--CONCEPT_SYNONYM.csv
    |--DRUG_STRENGTH.csv
    |--VOCABULARY.csv
    |--CONCEPT_CLASS.csv
    |--CONCEPT_RELATIONSHIP.csv
    |--DOMAIN.csv
    |--RELATIONSHIP.csv
```

## Metadata text files.

Two meta data files with names: "src_desc.txt" and "provenance_info.txt" must be placed in the `meta` folder.

### src_desc.txt

This file contains a description of the source data origin and purpose for collection. The description may contain a summary of the period of time that is expected to be covered by this dataset.

###

Please see [here](https://github.com/OHDSI/CommonDataModel) for the OMOP CDM repo. Choose the appropraite DDL for your database engine and instantionate a database with the tables. We will use an example with postgresql:

```sql
psql -h localhost -p 5432 -U username -f ../CommonDataModel/PostgreSQL/OMOP\ CDM\ postgresql\ ddl.txt cdm
```

## Omopification

You can now run this package by calling:

```r
install.packages("remotes")
remotes::install_github("cchic/icnarc2omop")
omopify_xml(
  xml_path = "./icnarc_xml",
  vocab_path = "./vocab",
  nhs_trust = "University College London Hospital",
  source_description = "./meta/provenance_info.txt",
  database_name = "cdm",
  database_engine = "postgres",
  host_name = "localhost",
  port_no = 5432,
  username = "username") {
```

You will be asked for your password to connect to the database. If all goes well. You should now have a lovely barebones set up of OMOP. Enjoy these magical good times.
