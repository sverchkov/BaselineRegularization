#' Get relevant tables from a database
#'
#' This is a weapper for dplyr::tbl that adds an explanatory flogger log message if dbplyr is not loaded.
#' @param con A DBI Connection obtained with DBI::dbConnect()
#' @param table The name of the table to extract
#' @return The table
#' @import futile.logger
#' @import dplyr
getDBTable <- function( con, table ){

  if( !requireNamespace( "dbplyr", quietly = T ) )
    flog.error( "Could not find the 'dbplyr' package, loading database tables will likely fail without it." )

  # Return
  ftry( tbl( con, table ) )
}
