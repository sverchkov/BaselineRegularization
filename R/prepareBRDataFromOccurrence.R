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
#' @param condition_id The column to use for condition ids (uses non-standard evaluation), condition_concept_id by default.
#' @param person_id The column to use for person ids (uses non-standard evaluation), person_id by default.
#' @return An object containing the matrices X, Z, y
#' @import futile.logger
#' @import dplyr
#' @import rlang
#' @export
prepareBRDataFromOccurrence <- function( con = NULL
                                       , drug_exposure = "drug_exposure"
                                       , condition_occurrence = "condition_occurrence"
                                       , visit_occurrence = "visit_occurrence"
                                       , event
                                       , tying = "occurrence"
                                       , risk_window = 0
                                       , minimum_duration = 0
                                       , drug_id = `!!`(drug_sym)
                                       , condition_id = `!!`(condition_sym)
                                       , person_id = `!!`(person_sym) )
{
  drug_sym <- enexpr( drug_id )
  condition_sym <- enexpr( condition_id )
  person_sym <- enexpr( person_id )

  if ( !( tying %in% c("occurrence", "interval" ) ) ) {
    stop( flog.fatal( "Invalid 'tying' parameter (%s) supplied.", tying ) )
  }

  drug_exposure <- getTable( con, drug_exposure, "drug exposure" )
  condition_occurrence <- getTable( con, condition_occurrence, "condition occurrence" )
  visit_occurrence <- getTable( con, visit_occurrence, "visit occurrence" )

  # Derive observation periods
  flog.trace("Inferring observation periods")
  observation_periods <- inferObservationPeriods( drug_exposure,
                             condition_occurrence,
                             visit_occurrence,
                             patient_id = !!person_sym )

  # Ensure correct types
  minimum_duration <- as.integer( minimum_duration )

  # Be explicit about the columns we expect, filter observation periods
  observation_periods <- observation_periods %>%
    transmute( observation_period_id = !!person_sym,
               person_id = !!person_sym,
               observation_period_start_date,
               observation_period_length = !!observation_period_end_date_sym - !!observation_period_start_date_sym + 1L ) %>%
    filter( observation_period_length >= minimum_duration )

  # Set up event table
  flog.trace("Preparing the event table")

  events <- getEventsFromOccurrence(
    drug_exposure = drug_exposure,
    condition_occurrence = condition_occurrence,
    observation_periods = observation_periods,
    event = event,
    risk_window = risk_window,
    drug_sym = drug_sym,
    condition_sym = condition_sym,
    person_sym = person_sym)

  flog.trace("Handing over the event table")

  if( "tbl_sql" %in% class( events ) )
    flog.trace("Event table query:", events %>% explain(), capture = T )

  if( "SQLiteConnection" == class( con ) ||
      ( ( "tbl_dbi" %in% class( events ) ) &&
        ( "src_dbi" %in% class( events$src ) ) &&
        ( "SQLiteConnection" == class( events$src$con ) ) ) ){
    flog.warn( "SQLite detected, using alternate implementation." )

    # Return
    prepareBRDataFromEventsDBI( events, event, tying )
  } else {

    # Return
    prepareBRDataFromEvents( events, event, tying )
  }
}
