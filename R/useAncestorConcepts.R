#' Use ancestor concepts
#'
#' Use an ancestry table to add a column of ancestor concepts to a record table
#' This is basically a wrapper for [dplyr::left_join], but only keeps one column of interest from the ancestry table
#'
#' This function uses tidy-eval non-standard evaluation for column names.
#'
#' @param record_table The table to augment
#' @param ancestors The table defining ancestry relations
#' @param record_table_column The column in `record_table` to be matched to ancestors
#' @param ancestor_column The column with which to augment `record_table`
#' @param descendant_column The column which to match to `record_table_column`
#' @param copy Passthrough parameter to [dplyr::left_join], needed when `record_table` and `ancestors` have different
#' sources
#' @author Yuriy Sverchkov
#' @import dplyr
#' @export
useAncestorConcepts <- function ( record_table,
                                  ancestors,
                                  record_table_column = !!br_symbol$concept_id,
                                  ancestor_column = !!br_symbol$ancestor_concept_id,
                                  descendant_column = !!br_symbol$descendant_concept_id,
                                  copy = FALSE )
{

  record_sym <- rlang::enexpr( record_table_column )
  ancestor_sym <- rlang::enexpr( ancestor_column )
  descendant_sym <- rlang::enexpr( descendant_column )

  by_map <- deparse( descendant_sym )
  names( by_map ) <- deparse( record_sym )

  left_join(
    record_table,
    ancestors %>% distinct( !!descendant_sym, !!ancestor_sym ),
    by = by_map, copy = copy )
}
