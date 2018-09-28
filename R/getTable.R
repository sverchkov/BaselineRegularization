#' Get a table, possibly from a database
#'
#' Takes a connection, table or table name, and (optional) label, if the second parameter isn't a table name, it just
#' returns it, otherwise, announces that it is loading the table from the database and uses [dplyr::tbl()] to do that.
#' Also adds an explanatory message if dbplyr is not loaded.
#' @param con A DBI Connection obtained with [DBI::dbConnect()]
#' @param table A table or the name of the table to extract
#' @param label User-friendly name for the table (used in info message)
#' @return The table
#' @import futile.logger
#' @import dplyr
getTable <- function( con, table, label = deparse( rlang::enexpr( table ) ) ){

  flog.trace( "Interpreting %s.", label )

  if ( isSingleString( table ) ){
    flog.trace( "%s was determined to be a single string.", label )

    if ( !is.null( con ) ) {
      flog.trace( "A database connection was provided." )

      if( !requireNamespace( "dbplyr", quietly = T ) )
        flog.error( "Could not find the 'dbplyr' package, loading database tables will likely fail without it." )

      flog.info( msg = "Using %s table '%s' from the database.", label, table )

      # Return
      return (
        ftry( tbl( con, table ),
              error = function( e ){
                if ( grepl( "no such table", e ) ) NULL
                else stop( e )
              } ) )

    } else {
      flog.trace( "No valid DB connection provided, cannot infer table from %s, returning NULL.", label )
      return ( NULL )
    }

  } else {
    flog.trace( "%s is not a single string, hope that means it's a table, returning it as-is.", label )
    return ( table )
  }
}
