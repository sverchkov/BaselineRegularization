#' Attempt to execute a function on a data table, maybe collect
#'
#' If the table is from an sqlite connection, collects before executing function.
#'
#' @param the_table the table
#' @param connection_class the connection class to check
#' @return `the_table` after [dplyr::collect()] if the connection class matches, unchanged otherwise.
#' @import dplyr
#' @import futile.logger
collectIfDBIs <- function ( the_table, connection_class = "SQLiteConnection" ) {

  # Check for sqlite connection
  if( connection_class %in% class( the_table$src$con ) ) {
    flog.trace( "Detected %s connection collecting to avoid unsupported op", connection_class )
    collect( the_table )
  } else {
    the_table
  }
}
