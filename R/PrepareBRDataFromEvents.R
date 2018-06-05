#' Generate BR data from event table ( internal function )
#'
#' @author Yuriy Sverchkov
#' @import dplyr
prepareBRDataFromEvents <- function ( all_events, event, tying ){

  # Make events for the start of observation periods
  obs_start_events <- all_events %>%
    distinct( obs_period, observation_period_end_date, event_date = observation_period_start_date )

  # Build the column name of the ADE we're targeting (same as what 'spread' below will generate)
  concept_id_event = paste0( "concept_id_", event )

  # Make list of event times
  event_times <- all_events %>%
    # We just need the times
    select( obs_period, event_date, observation_period_end_date ) %>%
    # Add the start-of-observation events
    dplyr::union( obs_start_events ) %>%
    # Get distinct
    distinct() %>%
    # Sort
    arrange( obs_period, event_date ) %>%
    # Get interval numbering
    mutate( interval_number = row_number( ) ) %>%
    # Group by observation periods
    group_by( obs_period ) %>%
    # Get interval end time
    mutate( interval_end = lead( event_date ) ) %>%
    # Replace NA by observation period end
    mutate( interval_end = ifelse( is.na( interval_end ), observation_period_end_date, interval_end ) ) %>%
    # Compute interval lengths
    mutate( interval_length = interval_end - event_date ) %>%
    # Ungroup
    ungroup() %>%
    # Clean up
    select( obs_period, event_date, interval_number, interval_length )

  # Get sperse representations to prepare feature matrices
  feature_indeces <- all_events %>%
    # Features don't contain the target event
    filter( concept_id != event ) %>%
    # Get event features
    inner_join( event_times, by = c( obs_period = "obs_period", event_date = "event_date" ) ) %>%
    # Get a dense numbering of drugs
    mutate( drug_number = dense_rank( concept_id ) )

  # ADE occurences
  ade_intervals <- all_events %>%
    filter( concept_id == event ) %>%
    # We only need obs period and event date
    select( obs_period, event_date ) %>%
    # Get interval numbers
    inner_join( event_times, by = c( obs_period = "obs_period", event_date = "event_date" ) ) %>%
    # We only need interval numbers
    select( interval_number ) %>%
    # Sort by intervals
    arrange( interval_number )

  ##
  ## Everything above this point didn't require executing the SQL if we were working in a db
  ##

  # We'll be referring to the number of intervals a lot
  number_of_intervals <- (
    event_times %>%
      summarize( n_intervals = max( interval_number ) ) %>%
      collect()
  )$n_intervals

  # Interval lengths
  interval_details <- event_times %>% select( interval_length, obs_period ) %>% collect()

  # Build feature matrix

  feature_indeces <- feature_indeces %>% collect()

  drug_vector <- feature_indeces$drug_number

  X <- sparseMatrix( i = feature_indeces$interval_number
                     , j = drug_vector
                     , x = feature_indeces$event_flag
                     , dims = c( number_of_intervals, max( drug_vector ) ) )

  # Fill exposure matrix (we marked the start of each exposure with a 1 and the day after the end with a -1 above. By
  # adding the value of the previous cell to each cell we get 1s in every interval during which the patient is exposed )
  X <- apply( X, 2, cumsum )

  # Parameter tying determines the Z matrix
  Z = switch( tying
              , occurrence = { # Occurence tying

                start_intervals <- obs_start_events %>%
                  # Get interval numbers
                  inner_join( event_times, by = c( obs_period = "obs_period", event_date = "event_date" ) ) %>%
                  # We only need interval numbers
                  select( interval_number )

                z_elements <- ade_intervals %>% union( start_intervals ) %>%
                  # Sort by intervals
                  arrange( interval_number ) %>%
                  # Get distance to next break
                  mutate( tie_length = lead( interval_number, default = number_of_intervals ) - interval_number ) %>%
                  collect()

                # Make the diagonal matrix
                bdiag( Map( function( x ) rep( 1, x ), z_elements$tie_length ) )
              }
              , interval = Diagonal( number_of_intervals ) ) # Interval tying

  # Get ade_intervals
  ade_intervals <- ade_intervals %>% collect()

  # Return
  list(
    X = X,
    Z = Z,
    l = as.numeric( interval_details$interval_length ), # Conversion to numeric from difftime
    n = as.vector( sparseVector( x = 1, i = ade_intervals$interval_number, length = number_of_intervals ) ),
    patients = interval_details$obs_period
  )
}
