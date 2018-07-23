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

  if ( !is.null( con ) ){
    observation_period <- getTable( con, observation_period, "observation period" )
    drug_era <- getTable( con, drug_era, "drug era" )
    condition_era <- getTable( con, condition_era, "condition era" )
  }

  if ( ! independent_observation_periods )
    stop( flog.fatal( "Current implementation only supports independent observation periods." ) )
  else {
    working_observation_periods <- observation_period %>%
      mutate( observation_period_length = 1L + observation_period_end_date - observation_period_start_date ) %>%
      filter( observation_period_length >= minimum_duration )
  }

  #drug_era_events <- inner_join( working_observation_periods, drug_era, by = c( person_id = "person_id" ) )

  # Risk window expansion
  working_drug_era =
    if ( risk_window > 0 ){
      mutate( drug_era_events, drug_era_end_date = drug_era_end_date + risk_window )
      # Check if we ended up merging drug eras
      warning("Risk window expansion may have introduced drug era overlaps")
    } else drug_era

  drug_start_events <- working_drug_era %>%
    select( event_era_id = drug_era_id
          , person_id
          , concept_id = drug_concept_id
          , event_date = drug_era_start_date ) %>%
    mutate( event_flag = 1L )

  drug_end_events <- working_drug_era %>%
    mutate( event_date = drug_era_end_date + 1L ) %>%
    select( event_era_id = drug_era_id
          , person_id
          , concept_id = drug_concept_id
          , event_date ) %>%
    mutate( event_flag = -1L )
  # Code notes on the above:
  # We cast 1 to integer when adding to the date because dbplyr converts numerals to decimals by default
  # We add 1 to the end date since the "event" is "no longer being exposed," this also folds in with how we turn this
  # into a feature table later (the corresponding interval becomes the first to have a 0 for that drug after a run of
  # 1s).
  # For some reason doing the addition in the select statement gives an "object not found" error, so that's why it's
  # in a mutate statement.

  condition_events <- condition_era %>%
    filter( condition_concept_id == response_event ) %>%
    select( event_era_id = condition_era_id
          , person_id
          , concept_id = condition_concept_id
          , event_date = condition_era_start_date ) %>%
    mutate( event_flag = 1L )

  # Get events associated with observation periods
  all_events <- # combine events
    drug_start_events %>% union_all( drug_end_events ) %>% union_all( condition_events ) %>%
    # join with observation periods
    inner_join( working_observation_periods, by = c( person_id = "person_id" ) ) %>%
    # Compute days since observation period start
    mutate( event_day = event_date - observation_period_start_date ) %>%
    # filter to events falling within observation periods
    filter( event_day >= 0, event_day <= observation_period_length )

  # Return
  prepareBRDataFromEvents( all_events, response_event, tying )
}
