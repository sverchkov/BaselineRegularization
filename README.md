BaselineRegularization
=======================

[![Build Status](https://travis-ci.com/sverchkov/BaselineRegularization.svg?branch=master)](https://travis-ci.com/sverchkov/BaselineRegularization) [![codecov](https://codecov.io/gh/sverchkov/BaselineRegularization/branch/master/graph/badge.svg)](https://codecov.io/gh/sverchkov/BaselineRegularization)


Introduction
------------

### Installation

The package can be installed from github with

```r
devtools::install_github("sverchkov/BaselineRegularization", build_vignettes = TRUE)
```

### Tutorial

The R vignettes include a tutorial that covers most features of the package, these can be accessed with
```r
browseVignettes("BaselineRegularization")
```

### Example

```r
# Load the package
library("BaselineRegularization")

# Connect to Database, e.g. postgres [1]
con <- DBI::dbConnect( RPostgreSQL::PostgreSQL()
                     , host = "localhost"
                     , user = "user"
                     , dbname = "omop_example"
                     , password = rstudioapi::askForPassword("Database Password") )

# Define the Event of interest
event = 4110956 # The concept_id for "Acute myocardial infarction NOS"

# Extract relevant data
br_data <- prepareBRData( con, response_event = event )

# Parametrize task
parameters <- defineBRParameters()

# Fit model
fit <- fitBaselineRegularization( br_data, parameters )

# Show results (beta coefficients)
getCoefficients( fit )

```

1. See [this guide](https://db.rstudio.com/dplyr/#connecting-to-the-database) for a more comprehensive overview of database connection. We use `dplyr` under the hood.

Technology
----------

BaselineRegularization is an R package.

System Requirements
-------------------

Requires R (version 3.0.0 or greater).

Dependencies
------------

Always required:

 * rlang (>= 0.2)
 * Matrix (>= 1.2)
 * dplyr (>= 0.7)
 * tidyr (>= 0.8)
 * glmnet (>= 2.0)
 * futile.logger (>= 1.4)

Required for database access:

 * DBI (>= 1.0.0),
 * dbplyr (>= 1.2),
 * Database drivers, depending on the database being accessed, e.g.
    * RPostgreSQL
    * RSQLite

Required for building the documentation:

 * knitr
 * rmarkdown

Required for testing:

 * testthat (>= 2.0.0)


License
-------

BaselineRegularization is licensed under Apache License 2.0

Development
-----------

BaselineRegularization is being developed in R Studio.


We use the [GitHub issue tracker](https://github.com/sverchkov/BaselineRegularization/issues) for bugs and feature requests


References
----------

Original papers describing the underlying algorithms:

 * Kuang, Z. et al. (2016).
   [Computational drug repositioning using continuous self-controlled case series.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5351812/) _In: Proceedings of the 22nd
ACM SIGKDD International Conference on Knowledge Discovery
and Data Mining._ ACM, pp. 491-500.
 * Kuang, Z. et al. (2017).
   [Pharmacovigilance via Baseline Regularization with Large-Scale Longitudinal Observational Data.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5945223/)
_In: Proceedings of the 23rd ACM SIGKDD International Conference
on Knowledge Discovery and Data Mining._ ACM, pp. 1537-1546.

[OHDSI Symposium 2018 poster](https://drive.google.com/drive/folders/1DBPJuD1pnXc6LPYqB4fpy7ohI30Uo4bR)
