
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle
Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/)
<!-- badges: end -->

## Overview

The role of this package is to help convert the most elementary
components of the ICNARC XML to OMOP CDM. This facilitates the set up of
an OMOP style CDM, acting as a base from which users can then populate
with data from their local EHR. This was a somewhat non-trivial task,
since the ICNARC specification is episode centric, and the OMOP CDM is
patient centric. This necessitated a number of opinionated design
decisions, especially in places where inforamtion of a person’s hospital
journey is lacking. These decisions may not work universally for all
study designs.

## Installation

``` r
# install directly from github with
remotes::install_github("cc-hic/icnarc2omop")
```

## Prerequisites

  - A running target database that is *empty*. PostgreSQL and MySQL are
    currently supported.
  - `R` (`R` is mandatory, R Studio is useful)
  - The ICNARC xmls you wish to convert
  - A text file describing the ETL process that generated the ICNARC
    files (for data provenance)
  - A download of the vocabuaries you would like to use from
    (Athena)\[<http://athena.ohdsi.org/>\]
  - A PC with at least 8Gb of memory (moving the OMOP vocabularies
    through memory is the main bottleneck). If this is a problem, please
    let me know and I will write a chunking function to mitigate this
    issue. You could also load the vocabularies yourself, and omit that
    functionality from the muncher.

## Usage

Before using this package, you will need to:

1.  Set up a new empty database (e.g. `CREATE DATABASE omop;`)
2.  Create a project folder and populate it with the necessary files.
    They *MUST* follow this convention *EXACTLY*:

<!-- end list -->

    |--xml/
        |--icnarc01.xml <- names must be unequviocal in order.
        |--icnarc02.xml
    |--meta/
        |--src_desc.txt
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

## Metadata text file.

“src\_desc.txt” must be placed in the `meta` folder. This file contains
a description of the source data origin and purpose for collection. The
description may contain a summary of the period of time that is expected
to be covered by this dataset.

## Omopification

You can now run the muncher by calling:

``` r
library(icnarc2omop)

omopify_xml(
  project_path = ".",
  nhs_trust = "Demo St. Elsewhere",
  cdm_version = "5.3.1",
  database_name = "omop",
  database_engine = "postgres",
  host_name = "localhost",
  port_no = 5432,
  username = "db username goes here"
  )
```

You will be asked for your password to connect to the database. If all
goes well. You should now have a lovely barebones set up of OMOP. Enjoy
these magical good times.
