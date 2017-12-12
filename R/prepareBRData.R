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
prepareBRData <- function ( con = NULL
                          , observation_period = NULL
                          , drug_era = NULL
                          , condition_era = NULL
                          , event
                          , tying = "occurence"
                          , risk_window = 0
                          , minimum_duration = 0
                          , independent_observation_periods = TRUE ){

  if ( !is.null( con ) ){

    tables <- getDBTables( con )

    if ( !is.null( observation_period ) ) warning( "observation_period table will be read from the database")
    observation_period <- tables$observation_period

    if ( !is.null( drug_era ) ) warning( "drug_era table will be read from the database")
    drug_era <- tables$drug_era

    if ( !is.null( condition_era ) ) warning( "condition_era table will be read from the database" )
    condition_era <- tables$condition_era

  } else {
    if ( is.null( observation_period ) || is.null( drug_era ) || is.null( condition_era ) )
      stop( "Either a database connection or data tables must be provided." )
  }

  if ( ! independent_observation_periods )
    stop( "Current implementation only supports independent observation periods." )
  else {
    working_observation_periods <- filter( observation_period, observation_period_end_date - observation_period_end_date >= minimum_duration )
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
    mutate( event_flag = 1 )

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

  # Get features
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

  # Make feature matrix
  features <- all_events %>%
    # Clean things up a bit
    select( -event_era_id, -person_id, -observation_period_id, -observation_period_start_date, -period_type_concept_id ) %>%
    # This is where we have to move things to memory
    collect() %>%
    # get a 1-column-per-feature representation
    spread( key = concept_id, value = event_flag, fill = as.integer(0), sep = "_" ) %>%
    # merge rows that correspond to the same date in the same observation period: first group
    group_by( obs_period, event_date, observation_period_end_date ) %>%
    # Then sum
    summarize_all( sum ) %>%
    # Group by observation periods
    group_by( obs_period ) %>%
    # sort by event date within each observation period
    arrange( obs_period, event_date ) %>%
    # compute interval lengths
    mutate( interval_length = lead( event_date, default = min( observation_period_end_date ) ) - event_date )

  # Note:
  # This process does not generate an interval for the time period between the beginning of the observation period and
  # the first event.


  # Get the column name of the event we're targeting
  concept_id_event = paste0( "concept_id_", event )

  # Get the event occurence vector
  n <- features[[ concept_id_event ]]
  features[[ concept_id_event ]] = NULL

  # Fill exposure matrix (we marked the start of each exposure with a 1 and the day after the end with a -1 above. By
  # adding the value of the previous cell to each cell we get 1s in every interval during which the patient is exposed )
  features <- features %>%
    mutate_at( vars( starts_with( "concept_id_" ) ), cumsum )

  if ( tying == "occurence" )
    stop( "occurence tying not implemented yet" )
  if ( tying == "interval" )
    Z = Diagonal( length( n ) )

  # Return
  list(
    X = data.matrix( features %>% select( starts_with( "concept_id" ) ) ),
    Z = Z,
    l = as.numeric( features$interval_length ), # Conversion to numeric from difftime
    n = n,
    patients = features$obs_period
  )
}
