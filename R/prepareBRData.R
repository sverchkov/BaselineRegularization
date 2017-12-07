#' Create data object
#'
#' Creates the data object for running Baseline Regularization
#' @param observation_period A dataframe-like view of the OMOP OBSERVATION_PERIOD table
#' @param drug_era A dataframe-like view of the OMOP DRUG_ERA table
#' @param condition_era A dataframe-like view of the OMOP CONDITION_ERA table
#' @param condition The condition_concept_id of the condition of interest
#' @param tying Parameter tying mode, "interval", or "occurence" (default)
#' @param risk_window The number of days right after a drug era during which the patient is considered still under exposure
#' @param minimum_duration The number of days a patient must be under observation to be included in the analysis
#' @param independent_observation_periods Whether to treat distinct observation periods from one patient as distinct "patients"
#' @return An object containing the matrices X, Z, y
#' @export
prepareBRData <- function ( observation_period
                          , drug_era
                          , condition_era
                          , event
                          , tying = "occurence"
                          , risk_window = 0
                          , minimum_duration = 0
                          , independent_observation_periods = TRUE ){

  if ( ! independent_observation_periods )
    stop( "Current implementation only supports independent observation periods." )
  else {
    working_observation_periods <- filter( observation_period, observation_period_end_date - observation_period_end_date >= minimum_duration )
  }

  drug_era_events <- inner.join( working_observation_periods, drug_era, by = c( person_id = "person_id" ) )

  if ( risk_window > 0 ){
    # Risk window expansion
    drug_era_events <- mutate( drug_era_events, drug_era_end_date = drug_era_end_date + risk_window )
    # Check for each drug
  }

  drug_start_events <-
    drug_era_events %>%
    filter( drug_era_start_date >= observation_period_start_date, drug_era_start_date <= observation_period_end_date ) %>%
    select( event_era_id = drug_era_id
          , event_type = "drug"
          , event_flag = 1
          , person_id
          , concept_id = drug_concept_id
          , event_date = drug_era_start_date
          , observation_period_id
          , observation_period_start_date
          , observation_period_end_date )

  drug_end_events <-
    drug_era_events %>%
    select( event_era_id = drug_era_id
          , event_type = "drug"
          , event_flag = -1
          , person_id
          , concept_id = drug_concept_id
          , event_date = drug_era_end_date + 1 # Check that this is how you add a day
          , observation_period_id
          , observation_period_start_date
          , observation_period_end_date ) %>%
    filter( event_date >= observation_period_start_date, event_date <= observation_period_end_date )

  condition_events <-
    condition_era %>%
    filter( condition_concept_id == event ) %>%
    inner.join( working_observation_periods, by = c( person_id = "person_id" ) ) %>%
    select( event_era_id = condition_era_id
          , event_type = "condition"
          , event_flag = 1
          , person_id
          , concept_id = condition_concept_id
          , event_date = condition_era_start_date
          , observation_period_id
          , observation_period_start_date
          , observation_period_end_date ) %>%
    filter( event_date >= observation_period_start_date, event_date <= observation_period_end_date )

  # bind rows
  features <- bind_rows( drug_start_events, drug_end_events, condition_events ) %>%
    mutate( obs_period = dense_rank( observation_period_id ) ) %>%
    spread( key = concept_id, value = event_flag, fill = 0, sep = "_" ) %>%
    group_by( obs_period ) %>%
    arrange( event_date, .by_group = TRUE ) #%>%
    #mutate( interval = seq_along( event_date ) ) # May not need this enumeration

  concept_id_event = paste0( "concept_id_", event )

  n <- features[[ concept_id_event ]]
  features[[ concept_id_event ]] = NULL

  # features <- select( -event_era_id
  #                   , -person_id
  #                   , -event_date
  #                   , -observation_period_id
  #                   , -observation_period_start_date
  #                   , -observation_period_end_date ) %>%
  features <- features %>%
    group_by( subject ) %>%
    mutate_at( starts_with( "concept_id_" ), funs( . + lag( . ) ) )

  X_transpose <- features %>% ungroup() %>% select( starts_with( "concept_id" ) )

  interval_lengths <-
    features %>%
    select( interval_length = lead( event_date, default = observation_period_end_date ) - event_date ) %>%
    ungroup()

  period_index <- features %>% ungroup() %>% select( obs_period )

  if ( tying == "occurence" )
    stop( "occurence tying not implemented yet" )
  if ( tying == "interval" )
    Z = Diagonal( length( n ) )

  # Return
  list(
    X = t( Matrix( X_transpose ) ),
    Z = Z,
    l = interval_lengths$interval_length,
    n = n,
    patients = period_index$obs_period
  )
}
