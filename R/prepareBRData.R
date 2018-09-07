#' Create data object based on era tables
#'
#' Creates the data object for running Baseline Regularization
#' @param con A DBI Connection to an OMOP CDM database used if the OMOP tables are provided as table names.
#' @param observation_period Either the name of the `observation_period` table in the database specified by
#' `con` or the table itself in a dataframe-like R object. If this is specified it defines the observation periods; if
#' it is not, the observation periods are inferred from the other tables in the data, assuming one all-encompassing
#' observation period per patient.
#' @param drug_era A dataframe-like view or the table name of the OMOP DRUG_ERA table
#' @param condition_era A dataframe-like view or the table name of the OMOP CONDITION_ERA table
#' @param drug_exposure Either the name of the `drug_exposure` table in the database specified by `con` or the
#' table itself in a dataframe-like R object
#' @param condition_occurrence Either the name of the `condition_occurrence` table in the database specified by `con`
#' or the table itself in a dataframe-like R object
#' @param visit_occurrence (optional) Either the name of the `visit_occurrence` table in the database specified by `con`
#' or the table itself in a dataframe-like R object, only used to infer observation periods if needed.
#' @param response_event The condition_concept_id of the condition of interest
#' @param tying Parameter tying mode, "interval", or "occurence" (default)
#' @param risk_window The number of days right after a drug era during which the patient is considered still under
#' exposure.
#' @param minimum_duration The number of days a patient must be under observation to be included in the analysis.
#' @param drug_concept_processor A processor function for drug concepts
#' @param condition_concept_processor A processor function for conditions
#' @return An object containing the matrices X, Z, y
#'
#' @author Yuriy Sverchkov
#'
#' @import futile.logger
#' @import dplyr
#' @export
prepareBRData <- function ( con = NULL
                          , observation_period = "observation_period"
                          , drug_era = "drug_era"
                          , condition_era = "condition_era"
                          , drug_exposure = "drug_exposure"
                          , condition_occurrence = "condition_occurrence"
                          , visit_occurrence = "visit_occurrence"
                          , response_event
                          , tying = "occurrence"
                          , risk_window = 0L
                          , minimum_duration = 0L
                          , drug_concept_processor = passthroughConceptProcessor
                          , condition_concept_processor = passthroughConceptProcessor )
{
  # Check valuse and ensure types

  if ( !( tying %in% c("occurrence", "interval" ) ) )
    stop( flog.fatal( "Invalid 'tying' parameter supplied (%s).", tying ) )

  minimum_duration <- as.integer( minimum_duration )

  # Get tables

  observation_period <- getTable( con, observation_period, "observation period" )
  drug_era <- getTable( con, drug_era, "drug era" )
  condition_era <- getTable( con, condition_era, "condition era" )
  drug_exposure <- getTable( con, drug_exposure, "drug exposure" )
  condition_occurrence <- getTable( con, condition_occurrence, "condition occurrence" )

  if ( is.null( observation_period ) ){ # Infer observation periods

    visit_occurrence <- getTable( con, visit_occurrence, "visit occurrence" )

    flog.debug("Inferring observation periods")
    observation_period <- inferObservationPeriods( drug_era,
                                                   condition_era,
                                                   drug_exposure,
                                                   condition_occurrence,
                                                   visit_occurrence,
                                                   patient_id = !!br_symbol$person_id ) %>%
      mutate( observation_period_id = !!br_symbol$person_id )
  }

  working_observation_periods <- observation_period %>%
    mutate( observation_period_length =
              1L + !!br_symbol$observation_period_end_date - !!br_symbol$observation_period_start_date ) %>%
    filter( !!br_symbol$observation_period_length >= minimum_duration ) %>%
    compute()

  # Get drug durations from drug eras
  drug_duration <-
    if ( is.null( drug_era ) ){
      getDrugDurationsFromExposure(
        drug_concept_processor( drug_exposure,
                                record_table_column = "drug_concept_id",
                                out_column = "drug_concept_id" ) %>% compute(),
        working_observation_periods )
    } else {
      inner_join(
        drug_concept_processor( drug_era,
                                record_table_column = "drug_concept_id",
                                out_column = "drug_concept_id" ),
        working_observation_periods,
        by = "person_id" ) %>%
        filter( !!br_symbol$drug_era_start_date >= !!br_symbol$observation_period_start_date,
                !!br_symbol$drug_era_end_date < !!br_symbol$observation_period_end_date ) %>%
        transmute( !!br_symbol$observation_period_id,
                   !!br_symbol$observation_period_length,
                   concept_id = !!br_symbol$drug_concept_id,
                   drug_start_day = as.integer( !!br_symbol$drug_era_start_date - !!br_symbol$observation_period_start_date ),
                   drug_end_day = as.integer( !!br_symbol$drug_era_end_date - !!br_symbol$observation_period_end_date ) ) %>%
        mutate( drug_end_day = if_else( is.na( !!br_symbol$drug_end_day ),
                                        !!br_symbol$drug_start_day,
                                        !!br_symbol$drug_end_day ) + 1L )
    }

  # Get drug events
  drug_events <- getDrugEvents( drug_duration, risk_window )

  # Derive condition days
  condition_events <-
    if ( is.null( condition_era ) ){
      getConditionEvents(
        condition_concept_processor( condition_occurrence,
                                     record_table_column = "condition_concept_id",
                                     out_column = "condition_concept_id" ),
        working_observation_periods )
    } else {
      getConditionEvents(
        condition_concept_processor( condition_era,
                                     record_table_column = "condition_concept_id",
                                     out_column = "condition_concept_id" ),
        working_observation_periods,
        date_column = !!br_symbol$condition_era_start_date )
    }

  results <- Map( function( event_id ){

    condition_event_subset <- filter( condition_events, !!br_symbol$concept_id == event_id )
    if ( 1 > ( condition_event_subset %>% summarize( count = n() ) %>% head(1) %>% collect() )$count ){
      flog.warn( "Response event %s doesn't occur in the cohort. Failed to build data for it.", event_id )
      return ( NULL )
    }

    # Derive events
    events_table <- union_all( drug_events, filter( condition_events, !!br_symbol$concept_id == event_id ) )

    # Prepare data
    prepareBRDataFromEvents( events_table, event_id, tying )

  }, response_event )

  if ( length( response_event ) > 1 ) {
    flog.debug( "Got multiple response_event values, will produce a list of multiple data objects." )
    return ( results[ !sapply( results, is.null ) ] )
  } else {
    return ( results[[1]] )
  }
}
