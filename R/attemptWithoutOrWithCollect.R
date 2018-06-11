#' Attempt to execute a function on a data table, maybe collect
#'
#' If the table is from an sqlite connection, collects before executing function.
#'
#' @param the_table the table
#' @param the_function the function
#' @return `the_function( the_table )` if possible.
#' @import dplyr
#' @import futile.logger
attemptWithoutOrWithCollect <- function ( the_table, the_function ) {

  # Check for sqlite connection
  if( "SQLiteConnection" %in% class( the_table$src$con ) ) {
    flog.warn( "Detected SQLite Connection, avoiding unsupported op" )
    the_function( collect( the_table ) )
  } else {
    the_function( the_table )
  }
}
