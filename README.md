BaselineRegularization
=======================

[![Build Status](https://travis-ci.com/sverchkov/BaselineRegularization.svg?branch=master)](https://travis-ci.com/sverchkov/BaselineRegularization) [![codecov](https://codecov.io/gh/sverchkov/BaselineRegularization/branch/master/graph/badge.svg)](https://codecov.io/gh/sverchkov/BaselineRegularization)


Introduction
------------

Features
--------

Example
-------
```r

# Connect to Database, e.g. postgres [1]
con <- DBI::dbConnect( RPostgreSQL::PostgreSQL()
                     , host = "localhost"
                     , user = "user"
                     , dbname = "omop_example"
                     , password = rstudioapi::askForPassword("Database Password") )

# Define the Event of interest
event = 4110956 # The concept_id for "Acute myocardial infarction NOS"

# Extract relevant data
br_data <- prepareBRDataFromOccurrence( con, response_event <- event )

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

 * dplyr (Version 0.7.5 or greater)
 * glmnet
 * Matrix
 
Getting Started
---------------

### Installation
The package can be installed from github with
```r
devtools::install_github("sverchkov/BaselineRegularization")
```

Getting Involved
----------------

License
-------

BaselineRegularization is licensed under Apache License 2.0

Development
-----------

BaselineRegularization is being developed in R Studio.

Acknowledgements
----------------

TODO
