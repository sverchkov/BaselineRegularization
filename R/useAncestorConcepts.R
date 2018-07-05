#' Use ancestor concepts
#'
#' Use an ancestry table to add a column of ancestor concepts to a record table
#' This is basically a wrapper for [dplyr::left_join], but only keeps one column of interest from the ancestry table
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
                                  record_table_column = "concept_id",
                                  ancestor_column = "ancestor_concept_id",
                                  descendant_column = "descendant_concept_id",
                                  copy = FALSE )
{

  by_map <- descendant_column
  names( by_map ) <- record_table_column

  left_join(
    record_table,
    ancestors %>% distinct(
      `!!`(rlang::sym( descendant_column )),
      `!!`(rlang::sym( ancestor_column )) ),
    by = by_map, copy = copy )
}
