#' Use ancestor concepts
#'
#' Use an ancestry table to add a column of ancestor concepts to a record table
#'
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
