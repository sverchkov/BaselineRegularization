#' Create data object based on era tables
#'
#' Creates the data object for running Baseline Regularization
#' @param con A DBI Connection to an OMOP CDM database used if the OMOP tables are provided as table names.
#' @param observation_period A dataframe-like view or the table name of the OMOP OBSERVATION_PERIOD table
#' @param drug_era A dataframe-like view or the table name of the OMOP DRUG_ERA table
#' @param condition_era A dataframe-like view or the table name of the OMOP CONDITION_ERA table
#' @param response_event The condition_concept_id of the condition of interest
#' @param tying Parameter tying mode, "interval", or "occurence" (default)
#' @param risk_window The number of days right after a drug era during which the patient is considered still under
#' exposure.
#' @param minimum_duration The number of days a patient must be under observation to be included in the analysis.
#' @param independent_observation_periods Whether to treat distinct observation periods from one patient as distinct
#' "patients." Default value and only current accepted value is TRUE.
#' @return An object containing the matrices X, Z, y
#'
#' @author Yuriy Sverchkov
#'
#' @import futile.logger
#' @import dplyr
#' @export
prepareBRDataFromEras <- function ( con = NULL
                          , observation_period = "observation_period"
                          , drug_era = "drug_era"
                          , condition_era = "condition_era"
                          , response_event
                          , tying = "occurence"
                          , risk_window = 0
                          , minimum_duration = 0
                          , independent_observation_periods = TRUE ){

  if ( !( tying %in% c("occurrence", "interval" ) ) )
    stop( flog.fatal( "Invalid 'tying' parameter supplied (%s).", tying ) )

  observation_period <- getTable( con, observation_period, "observation period" )
  drug_era <- getTable( con, drug_era, "drug era" )
  condition_era <- getTable( con, condition_era, "condition era" )

  if ( ! independent_observation_periods )
    stop( flog.fatal( "Current implementation only supports independent observation periods." ) )
  else {
    working_observation_periods <- observation_period %>%
      mutate( observation_period_length =
                1L + !!br_symbol$observation_period_end_date - !!br_symbol$observation_period_start_date ) %>%
      filter( !!br_symbol$observation_period_length >= minimum_duration )
  }

  # Get drug durations from drug eras
  drug_durations <- inner_join( drug_era, working_observation_periods, by = "person_id" ) %>%
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

  # Get drug events
  drug_events <- getDrugEvents( drug_durations, risk_window )

  # Derive condition days
  condition_events <- getConditionEvents( condition_era, working_observation_periods, date_column = !!br_symbol$condition_era_start_date )

  # Derive events
  events_table <- union_all( drug_events, filter( condition_events, !!br_symbol$concept_id == response_event ) )

  # Return
  prepareBRDataFromEvents( events_table, response_event, tying )
}
