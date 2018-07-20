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
#' @param response_event The condition_concept_id of the event of interest
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
                                       , response_event
                                       , tying = "occurrence"
                                       , risk_window = 0
                                       , minimum_duration = 0
                                       , drug_id = `!!`(drug_sym)
                                       , condition_id = `!!`(condition_sym)
                                       , person_id = `!!`(person_sym) )
{
  # Get symbols
  drug_sym <- enexpr( drug_id )
  condition_sym <- enexpr( condition_id )
  person_sym <- enexpr( person_id )

  # Input validation
  if ( !( tying %in% c("occurrence", "interval" ) ) ) {
    stop( flog.fatal( "Invalid 'tying' parameter (%s) supplied.", tying ) )
  }

  # Get tables
  drug_exposure <- getTable( con, drug_exposure, "drug exposure" )
  condition_occurrence <- getTable( con, condition_occurrence, "condition occurrence" )
  visit_occurrence <- getTable( con, visit_occurrence, "visit occurrence" )

  # Infer observation periods
  flog.trace("Inferring observation periods")
  observation_period <- inferObservationPeriods( drug_exposure,
                                                  condition_occurrence,
                                                  visit_occurrence,
                                                  patient_id = !!person_sym )

  # Ensure correct type
  minimum_duration <- as.integer( minimum_duration )

  # Check DBs
  for ( the_table in list( drug_exposure, condition_occurrence, observation_period ) ){
    if( "SQLiteConnection" == class( con ) ||
        ( ( "tbl_dbi" %in% class( the_table ) ) &&
          ( "src_dbi" %in% class( the_table$src ) ) &&
          ( "SQLiteConnection" == class( the_table$src$con ) ) ) ){
      flog.fatal( "SQLite detected, things will probably go badly." )
    }
  }

  # Be explicit about the columns we expect, filter observation periods
  observation_period <- observation_period %>%
    transmute( observation_period_id = !!person_sym,
               person_id = !!person_sym,
               observation_period_start_date,
               observation_period_length = !!observation_period_end_date_sym - !!observation_period_start_date_sym + 1L ) %>%
    filter( observation_period_length >= minimum_duration ) %>%
    compute() # We'll be reusing this so it's better to have a computed table

  # Derive drug durations (without risk window, possibly overlapping)
  drug_duration <- getDrugDurationsFromExposure( drug_exposure, observation_period )

  # Get drug events
  drug_events <- getDrugEvents( drug_duration, risk_window )

  # Derive condition occurrence days
  condition_events <- getConditionEvents( condition_occurrence, observation_period )

  # Derive events
  events_table <- union_all( drug_events, filter( condition_events, concept_id == response_event ) )

  # Prepare data
  prepareBRDataFromEvents( events_table, response_event, tying )
}
