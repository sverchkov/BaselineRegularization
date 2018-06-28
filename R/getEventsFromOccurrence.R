#' Make events table from occurrence information
#'
#' @import dplyr
#' @import futile.logger
getEventsFromOccurrence <- function( drug_exposure
                                   , condition_occurrence
                                   , visit_occurrence
                                   , event
                                   , risk_window
                                   , minimum_duration
                                   )
{
  # Ensure correct types
  minimum_duration = as.integer( minimum_duration )
  risk_window = as.integer( risk_window )

  # Derive observation periods
  observation_periods <-
    if( is.null( visit_occurrence ) )
      inferObservationPeriods( drug_exposure,
                               condition_occurrence,
                               patient_id = "person_id" )
    else
      inferObservationPeriods( drug_exposure,
                               condition_occurrence,
                               visit_occurrence,
                               patient_id = "person_id" )

  # Be explicit about the columns we expect
  observation_periods <- observation_periods %>%
    transmute( obs_period_id = person_id,
               person_id,
               observation_period_start_date = observation_period_start,
               observation_period_end_date = observation_period_end )

  flog.trace("Computing valid persons list")
  valid_persons <- observation_periods %>% select( person_id ) %>% compute()

  # Drug event window expansion
  drug_durations <- drug_exposure %>%
    inner_join( valid_persons, by = "person_id" ) %>%
    transmute( person_id
               , concept_id = drug_concept_id
               , drug_exposure_start_date
               , drug_exposure_end_date = pmax( drug_exposure_start_date, drug_exposure_end_date, na.rm = T )
    )

  if ( risk_window > 0 )
    drug_durations <- drug_durations %>%
    mutate( drug_exposure_end_date = drug_exposure_end_date + risk_window )

  # Make table in the db since it will be reused
  flog.trace("Computing intermediate drug durations table")
  # if( "tbl_dbi" %in% class( drug_durations ) ) drug_durations %>% explain()
  drug_durations <- drug_durations %>% compute()

  # Return value
  union_all( drug_durations %>%
    transmute( person_id, concept_id,
               event_date = drug_exposure_start_date,
               event_flag = 1L ),
    drug_durations %>%
      transmute( person_id, concept_id,
                 event_date = drug_exposure_end_date + 1L,
                 event_flag = -1L ) ) %>%
    union_all( condition_occurrence %>%
                 inner_join( valid_persons, by = "person_id" ) %>%
                 filter( condition_concept_id == event ) %>%
                 transmute( person_id, concept_id = condition_concept_id,
                            event_date = condition_start_date,
                            event_flag = 1L ) ) %>%
    inner_join( observation_periods, by = "person_id" ) %>%
    filter( event_date <= observation_period_end_date )

  # Code notes on the above:
  # We add 1 to the end date since the "event" is "no longer being exposed," this also folds in with how we turn this
  # into a feature table later (the corresponding interval becomes the first to have a 0 for that drug after a run of
  # 1s).
}
