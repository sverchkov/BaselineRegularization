#' Get Beta Coefficients from fit
#'
#' Function to conveniently get beta coefficients from the BR model in the form of a table, showing the concept_id and,
#' if available, name of the drug.
#'
#' @param fit the fit obtained from [fitBaselineRegularization].
#' @param con [DBI] connection for `names_table`.
#' @param names_table a table or the name of a database table (if con is provided) that matches the `concept_id` of the
#' drugs to more readable names.
#' @param id_var (NSE) the column in `names_table` that corresponds to drug `concept_id`.
#' @param name_var (NSE) the column in `names_table` that corresponds to drug name.
#' @param sort_by (NSE) the value by which to sort the beta coefficients.
#' @param keep_zeros whether to keep exact zeros in result.
#'
#' @return Table sorted as specified with columns Beta, concept_id, and (if possible) "Drug Name".
#'
#' @import dplyr
#' @export
#' @author Yuriy Sverchkov
getCoefficients <- function ( fit,
                              con = NULL,
                              names_table = "concept",
                              id_var = !!(br_symbol$concept_id),
                              name_var = !!(br_symbol$concept_name),
                              sort_by = desc( abs( !!(br_symbol$Beta) ) ),
                              keep_zeros = F )
{
  # Extract NSE symbols
  id_sym <- rlang::enexpr( id_var )
  name_sym <- rlang::enexpr( name_var )
  sort_sym <- rlang::enexpr( sort_by )

  # Try to get names table
  names_table <- getTable( con, names_table )

  # Get core result table
  result <- tibble( Beta = as.vector( fit$beta ), concept_id = fit$drug_concept_id )

  if ( !keep_zeros )
    result <- filter( result, !!br_symbol$Beta != 0 )

  result <- arrange( result, !!sort_sym )

  # Try to join with names
  if ( !is.null( names_table ) )
    result <-
      left_join( result, names_table, by = c( concept_id = deparse( id_sym ) ) ) %>%
      select( !!br_symbol$Beta, !!br_symbol$concept_id, `Drug Name` = !!name_sym )

  return ( result )
}
