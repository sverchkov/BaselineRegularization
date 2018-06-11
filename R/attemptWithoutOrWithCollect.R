#' Attempt to execute a function on a data table, maybe collect
#'
#' Tries to apply a function to a table, if that fails with an error, tries again after calling collect() on the table.
#' This is to get around functions that aren't supported in some DBs.
#'
#' @param the_table the table
#' @param the_function the function
#' @return `the_function( the_table )` if possible.
#' @import dplyr
#' @import futile.logger
attemptWithoutOrWithCollect <- function ( the_table, the_function ) {
  flog.debug("Entering attemptWithoutOrWithCollect")
  result <- tryCatch(
    the_function( the_table ),
    error = function( e ){
      flog.warn( "Caught %s, trying workaround.", as.character(e) )
      the_function( collect( the_table ) )
    }
  )
  flog.debug("Exiting attemptWithoutOrWithCollect")
  return ( result )
}
