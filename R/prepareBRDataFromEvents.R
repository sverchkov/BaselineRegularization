#' Generate BR data from event table ( internal function )
#'
#' @author Yuriy Sverchkov
#' @import dplyr
prepareBRDataFromEvents <- function ( all_events, event, tying ){

  all_events <- all_events %>% mutate( obs_period = dense_rank( obs_period_id ) )

  # Make events for the ends of observation periods
  obs_start_events <- all_events %>%
    distinct( obs_period, observation_period_length ) %>%
    mutate( event_day = 0L )

  # Make list of event times
  event_times <- all_events %>%
    # We just need the times
    distinct( obs_period, event_day, observation_period_length ) %>%
    # Add the start-of-observation events
    union( obs_start_events ) %>%
    # Get days to end of obs period for each event
    transmute( obs_period, event_day, days_to_end = observation_period_length - event_day ) %>%
    # Sort
    arrange( obs_period, event_day ) %>%
    # Get interval numbering
    mutate( interval_number = row_number( ) ) %>%
    # Group by observation periods
    group_by( obs_period ) %>%
    # Get next interval's days to end, catch unsupported DB op
    mutate( next_interval_days_to_end = lead( days_to_end ) ) %>%
    # Interval length = next interval days to end - this interval days to end (if not last)
    mutate( interval_length = ifelse( is.na( next_interval_days_to_end ), days_to_end, days_to_end - next_interval_days_to_end ) ) %>%
    # Ungroup
    ungroup() %>%
    # Clean up
    select( obs_period, event_day, interval_number, interval_length )

  # Get sperse representations to prepare feature matrices
  feature_indeces <- all_events %>%
    # Features don't contain the target event
    filter( concept_id != event ) %>%
    # Get event features
    left_join( event_times, by = c( obs_period = "obs_period", event_day = "event_day" ) ) %>%
    # Get a dense numbering of drugs
    mutate( drug_number = dense_rank( concept_id ) )

  # ADE occurences
  ade_intervals <- all_events %>%
    filter( concept_id == event ) %>%
    # We only need obs period and event date, and we don't count multiple occurrences per interval
    distinct( obs_period, event_day ) %>%
    # Get interval numbers
    inner_join( event_times, by = c( obs_period = "obs_period", event_day = "event_day" ) ) %>%
    # We only need interval numbers
    select( interval_number ) %>%
    # Sort by intervals
    arrange( interval_number )


  # We'll be referring to the number of intervals a lot
  number_of_intervals <- as.integer( (
    event_times %>%
      summarize( n_intervals = max( interval_number, na.rm = T ) ) %>%
      collect()
  )$n_intervals )

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
  X <- pmin( apply( X, 2, cumsum ), 1 )

  # Parameter tying determines the Z matrix
  Z = switch( tying
              , occurrence = { # Occurence tying

                start_intervals <- obs_start_events %>%
                  # Get interval numbers
                  inner_join( event_times, by = c( obs_period = "obs_period", event_day = "event_day" ) ) %>%
                  # We only need interval numbers
                  select( interval_number )

                # Sort by intervals
                z_elements <- ade_intervals %>% union( start_intervals ) %>%
                  arrange( interval_number ) %>%
                  # Get distance to next break
                  mutate( lead_interval = lead( interval_number ) ) %>%
                  mutate( lead_interval = ifelse( is.na( lead_interval ), number_of_intervals+1L, lead_interval ) ) %>%
                  mutate( tie_length = lead_interval - interval_number ) %>%
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
