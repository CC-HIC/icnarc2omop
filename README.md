# icnarc2omop

The role of this package is to help convert the most basic, elementary components of the ICNARC XML to OMOP CDM. This facilitates the set up of an OMOP style CDM, to while users can then populate with information from their EHR.

## Prerequisites

- A running database (I will use postgres in this demo)
- R and, preferably, R Studio
- Some ICNARC xmls
- A text file describing the ETL process for generating the ICNARC XML
- Downloaded CDM 6 vocabuaries from Athena

## Getting Started

Before using this package, you will need to:

1. Set up a new database, and populate it with the OMOP tables.
2. Create a project folder with the necessary files. I suggest this:

```
|--icnarc_xml/
    |--icnarc01.xml
    |--icnarc02.xml
|--meta/
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
