#' Create an ancestry concept processor
#'
#' Use an ancestry table to add a column of ancestor concepts to a record table
#'
#' @param concept_ancestor The table defining ancestry relations
#' @param concept_tbl A one-column table with the ancestor concepts to map (use either this or concept_list)
#' @param concept_list An array or list with the ancestor concepts to map (use either this or concept_tbl)
#' @param handle_remaining How to handle concepts not mapped to ancestors. "drop" (the default) removes the records,
#' "passthrough" passes the values through (only use if ancestor and descendant concepts are of the same type).
#' @param con The connection to use when specifying tables by their names in a database as strings
#' @param record_table_column The column in the record table on which to map ancestors
#' @param ancestor_column The ancestor column in `concept_ancestor`
#' @param descendant_column The descendant column in `concept_ancestor`
#' @param out_column The name to give the resulting column
#' @param copy Passthrough parameter to [dplyr::left_join], needed when tables have different sources
#' @return A function that operates on a table and adds `out_column` accordingly
#' @author Yuriy Sverchkov
#' @import dplyr
#' @import futile.logger
#' @import stats
#' @export
ancestorConceptProcessor <- function (
  concept_ancestor = "concept_ancestor",
  concept_tbl = NULL,
  concept_list = NULL,
  handle_remaining = "drop",
  con = NULL,
  record_table_column = "concept_id",
  ancestor_column = "ancestor_concept_id",
  descendant_column = "descendant_concept_id",
  out_column = "concept_id",
  copy = FALSE )
{
  # Get relevant symbols
  ancestor_sym <- as.symbol( ancestor_column )
  descendant_sym <- as.symbol( descendant_column )

  # Get concept ancestor table
  ancestor_tbl <- getTable( con, concept_ancestor ) %>%
    distinct( !!ancestor_sym, !!descendant_sym )

  # Figure out what to do with the concept_list/concept_df
  if ( is.null( concept_list ) && !is.null( concept_tbl ) ){ # Use concept_tbl

    # Get column
    column <- colnames( concept_tbl <- getTable( con, concept_tbl ) )
    if ( length( column > 1 ) ){
      column <- column[1]
      flog.warn( "ancestorConceptProcessor got a concept_tbl with multiple columns, using the first column, '%s'", column )
    }

    # Filter down table (is this efficient?)
    concept_tbl <- distinct( !!as.symbol(column) )

    # Filter down ancestor table
    ancestor_tbl <- inner_join( ancestor_tbl, concept_tbl, by = setNames( column, ancestor_column ), copy = copy )

  } else if ( !is.null( concept_list ) && is.null( concept_tbl ) ) { # Use concept_list

    ancestor_tbl <- ancestor_tbl %>% filter( ancestor_sym %in% concept_list )

  } else if ( !is.null( concept_list ) && !is.null( concept_tbl ) ) {

    warn( flog.error( "Both concept_list and concept_tbl provided to ancestorConceptProcessor. Using neither." ) )

  } # If using neither we just use everything in the ancestor table

  # Make the handler function
  handler = switch (
    EXPR = handle_remaining,
    passthrough = function ( tbl_in )
      mutate( tbl_in, if_else( is.na( ancestor_sym ), !!descendant_sym, !!ancestor_sym )),
    drop =, # Drop will be the default
    function ( tbl_in )
      filter( tbl_in, !is.na( ancestor_sym ) )
  )

  # Make the processor function
  function ( record_table, record_table_column = record_table_column, out_column = out_column ){
    record_table %>%
      left_join( ancestor_tbl, by = setNames( ancestor_column, record_table_column ), copy = copy ) %>%
      handler() %>%
      rename_at( vars( ancestor_column ), function (x) out_column ) # Using rename at to get around NSE nonsense
  }
}
