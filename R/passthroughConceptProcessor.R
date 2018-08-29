#' Passthrough concept processor
#'
#' A concept processor that just copies `record_table_column` to `out_column`
#'
#' @param record_table The table to process
#' @param record_table_column The record table column to process
#' @param out_column The column to which to write the processor result
#' @return `record_table` with `out_column` added, being a copy of `record_table_column`
#'
#' @import dplyr
passthroughConceptProcessor <- function (
  record_table,
  record_table_column = "concept_id",
  out_column = record_table_column )
{
  mutate( record_table, !!as.symbol(out_column) := !!as.symbol( record_table_column ) )
}
