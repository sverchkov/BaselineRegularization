BaselineRegularization
=======================

Introduction
------------

Features
--------

Example
-------
```r

# Connect to Database
# TODO

# Extract relevant data
br_data <- prepareBRData( data, event )

# Parametrize task
parameters <- defineBRParameters()

# Fit model
fit <- fitBaselineRegularization( br_data, parameters )

# Print results
# TODO

```

Technology
----------

BaselineRegularization is an R package.

System Requirements
-------------------

Requires R (version to be specified).

Dependencies
------------

 * dplyr (Version 0.7.5 or greater)
 * glmnet
 * Matrix
 
Getting Started
---------------

* Installation
* Package downloads

Getting Involved
----------------

License
-------

BaselineRegularization is licensed under Apache License 2.0

Development
-----------

BaselineRegularization is being developed in R Studio.

### Development status

(TODO: github+codecov.io development status widget)

Acknowledgements
----------------

TODO
