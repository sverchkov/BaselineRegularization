#' Create data object
#'
#' Creates the data object for running Baseline Regularization
#' @param con A DBI Connection to an OMOP CDM database. If it is provided, the observation_period, drug_era, and
#' condition_era tables will be read from the database and the corresponding parameters provided to this function will
#' be ignored. If it is not provided, the tables will be read from the parameters.
#' @param observation_period A dataframe-like view of the OMOP OBSERVATION_PERIOD table
#' @param drug_era A dataframe-like view of the OMOP DRUG_ERA table
#' @param condition_era A dataframe-like view of the OMOP CONDITION_ERA table
#' @param condition The condition_concept_id of the condition of interest
#' @param tying Parameter tying mode, "interval", or "occurence" (default)
#' @param risk_window The number of days right after a drug era during which the patient is considered still under
#' exposure.
#' @param minimum_duration The number of days a patient must be under observation to be included in the analysis.
#' @param independent_observation_periods Whether to treat distinct observation periods from one patient as distinct
#' "patients." Default value and only current accepted value is TRUE.
#' @return An object containing the matrices X, Z, y
#' @import tidyr
#' @import dplyr
#' @export
prepareBRDataFromEras <- function ( con = NULL
                          , observation_period = NULL
                          , drug_era = NULL
                          , condition_era = NULL
                          , event
                          , tying = "occurence"
                          , risk_window = 0
                          , minimum_duration = 0
                          , independent_observation_periods = TRUE ){

  if ( !( tying %in% c("occurrence", "interval" ) ) ) stop( "Invalid 'tying' parameter supplied." )
  if ( !is.null( con ) ){

    if ( !is.null( observation_period ) ) warning( "observation_period table will be read from the database")
    observation_period <- getDBTable( con, "observation_period" )

    if ( !is.null( drug_era ) ) warning( "drug_era table will be read from the database")
    drug_era <- getDBTable( con, "drug_era" )

    if ( !is.null( condition_era ) ) warning( "condition_era table will be read from the database" )
    condition_era <- getDBTable( con, "condition_era" )

  } else {
    if ( is.null( observation_period ) || is.null( drug_era ) || is.null( condition_era ) )
      stop( "Either a database connection or data tables must be provided." )
  }

  if ( ! independent_observation_periods )
    stop( "Current implementation only supports independent observation periods." )
  else {
    working_observation_periods <- filter( observation_period, observation_period_end_date - observation_period_start_date >= minimum_duration )
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
    mutate( event_flag = as.integer(1) )

  drug_end_events <- working_drug_era %>%
    mutate( event_date = drug_era_end_date + as.integer(1) ) %>%
    select( event_era_id = drug_era_id
          , person_id
          , concept_id = drug_concept_id
          , event_date ) %>%
    mutate( event_flag = as.integer(-1) )
  # Code notes on the above:
  # We cast 1 to integer when adding to the date because dbplyr converts numerals to decimals by default
  # We add 1 to the end date since the "event" is "no longer being exposed," this also folds in with how we turn this
  # into a feature table later (the corresponding interval becomes the first to have a 0 for that drug after a run of
  # 1s).
  # For some reason doing the addition in the select statement gives an "object not found" error, so that's why it's
  # in a mutate statement.

  condition_events <- condition_era %>%
    filter( condition_concept_id == event ) %>%
    select( event_era_id = condition_era_id
          , person_id
          , concept_id = condition_concept_id
          , event_date = condition_era_start_date ) %>%
    mutate( event_flag = as.integer(1) )

  # Get events associated with observation periods
  all_events <- # combine events
    drug_start_events %>% dplyr::union( drug_end_events ) %>% dplyr::union( condition_events ) %>%
    # join with observation periods
    inner_join( working_observation_periods, by = c( person_id = "person_id" ) ) %>%
    # filter to events falling within observation periods
    filter( event_date >= observation_period_start_date, event_date <= observation_period_end_date ) %>%
    # get a dense indexing of observation periods (may not be strictly necessary)
    mutate( obs_period = dense_rank( observation_period_id ) )

  # Bookkeeping: save the observation period mapping
  obs_id_map <- all_events %>% distinct( observation_period_id, obs_period )

  # Return
  prepareBRDataFromEvents( all_events, event, tying )
}
