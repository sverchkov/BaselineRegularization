#' Get relevant tables from a database
#'
#' @param con A DBI Connection obtained with DBI::dbConnect()
#' @return A list of 3 tables, specifically observation_period, drug_era, and condition_era (see the OMOP CDM for details)
#' @import dbplyr
getDBTables <- function( con ){
  # Return
  list( observation_period = tbl( con, "observation_period" )
      , drug_era = tbl( con, "drug_era" )
      , condition_era = tbl( con, "condition_era" ) )
}
