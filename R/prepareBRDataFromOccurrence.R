#' Create data object for baseline regularization
#'
#' Creates the data object for running Baseline Regularization
#' @param con A DBI Connection to an OMOP CDM database. If this is absent or NULL, tables must be provided as
#' dataframe-like R objects.
#' @param drug_exposure Either the name of the `drug_exposure` table in the database specified by `con` or the
#' table itself in a dataframe-like R object
#' @param condition_occurrence Either the name of the `condition_occurrence` table in the database specified by `con`
#' or the table itself in a dataframe-like R object
#' @param visit_occurrence Either the name of the `visit_occurrence` table in the database specified by `con` or the
#' table itself in a dataframe-like R object.
#' A patient will be considered observed from their first to their last visit.
#' If this parameter is NULL then the patient will be considered from their first to their last drug exposure and/or
#' condition occurence
#' @param event The condition_concept_id of the event of interest
#' @param tying Parameter tying mode, "interval", or "occurence" (default)
#' @param risk_window The number of days right after a drug exposure during which the patient is considered still under
#' exposure.
#' @param minimum_duration The number of days a patient must be under observation to be included in the analysis.
#' @param drug_id The column to use for drug ids (uses non-standard evaluation), drug_concept_id by default.
#' @param condition_id The column to use for condition ids (uses non-standard evaluation), condition_concept_if by default.
#' @param person_id The column to use for person ids (uses non-standard evaluation), person_id by default.
#' @return An object containing the matrices X, Z, y
#' @import futile.logger
#' @import dplyr
#' @export
prepareBRDataFromOccurrence <- function( con = NULL
                                       , drug_exposure = "drug_exposure"
                                       , condition_occurrence = "condition_occurrence"
                                       , visit_occurrence = "visit_occurrence"
                                       , event
                                       , tying = "occurrence"
                                       , risk_window = 0
                                       , minimum_duration = 0
                                       , drug_id = drug_concept_id
                                       , condition_id = condition_concept_id
                                       , person_id = person_id )
{

  if ( !( tying %in% c("occurrence", "interval" ) ) ) {
    stop( flog.fatal( "Invalid 'tying' parameter (%s) supplied.", tying ) )
  }

  if ( !is.null( con ) ){
    drug_exposure <- getTable( con, drug_exposure, "drug exposure" )
    condition_occurrence <- getTable( con, condition_occurrence, "condition occurrence" )
    visit_occurrence <- getTable( con, visit_occurrence, "visit occurrence" )
  }

  flog.trace("Preparing the event table")

  events <- getEventsFromOccurrence(
    drug_exposure = drug_exposure,
    condition_occurrence = condition_occurrence,
    visit_occurrence = visit_occurrence,
    event = event,
    risk_window = risk_window,
    minimum_duration = minimum_duration,
    drug_id = !!rlang::enexpr(drug_id),
    condition_id = !!rlang::enexpr(condition_id),
    person_id = !!rlang::enexpr(person_id))

  flog.trace("Handing over the event table")

  if( "tbl_sql" %in% class( events ) )
    flog.trace("Event table query:", events %>% explain(), capture = T )

  if( "SQLiteConnection" == class( con ) ||
      ( ( "tbl_dbi" %in% class( events ) ) &&
        ( "src_dbi" %in% class( events$src ) ) &&
        ( "SQLiteConnection" == class( events$src$con ) ) ) ){
    flog.warn( "SQLite detected, will do more work in memory" )

    # Return
    prepareBRDataFromEvents2( events, event, tying )
  } else {

    # Return
    prepareBRDataFromEvents( events, event, tying )
  }
}
