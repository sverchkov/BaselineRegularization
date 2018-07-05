#' Make events table from occurrence information (internal function)
#'
#' Make events table for [prepareBRDataFromEvents] from occurrence tables (rather than era tables).
#'
#' Drug exposure durations are calculated as: exposure end date - exposure start date (if end date is specified) or
#' according to `days_supply` (if specified) or 1 day if neither is specified, plus `risk_window`.
#'
#' @param drug_exposure A dataframs-like object correspondint to the OMOP CDM `drug_exposure` table
#' @param condition_occurrence A dataframs-like object correspondint to the OMOP CDM `condition_occurrence` table
#' @param visit_occurrence A dataframs-like object correspondint to the OMOP CDM `visit_occurrence` table
#' @param event The event of interest
#' @param risk_window See note on drug exposure duration calculation
#' @param minimum_duration The minimum duration that a patient needs to be under observation to be included in the
#' analysis.
#'
#' @author Yuriy Sverchkov
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
  minimum_duration <- as.integer( minimum_duration )

  lasting_risk <- Inf == risk_window
  if ( !lasting_risk ) risk_window <- as.integer( risk_window )

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

  # Be explicit about the columns we expect, filter observation periods
  observation_periods <- observation_periods %>%
    transmute( obs_period_id = person_id,
               person_id,
               observation_period_start,
               observation_period_length = observation_period_end - observation_period_start + 1L ) %>%
    filter( observation_period_length >= minimum_duration )

  # Drug event window expansion
  drug_durations <- drug_exposure %>%
    inner_join( observation_periods, by = "person_id" ) %>%
    mutate( drug_start_day = as.integer( drug_exposure_start_date - observation_period_start ) ) %>%
    filter( drug_start_day >= 0, drug_start_day <= observation_period_length ) %>%
    transmute( person_id,
               obs_period_id,
               observation_period_length,
               concept_id = drug_concept_id,
               drug_start_day,
               drug_end_day =
                 1L + drug_start_day +
                 if_else( is.na( drug_exposure_end_date ),
                          if_else( is.na( days_supply ), 0L, days_supply ),
                          as.integer( drug_exposure_end_date - drug_exposure_start_date ) )
    )

  if ( !lasting_risk && risk_window > 0 )
    drug_durations <- drug_durations %>%
      mutate( drug_end_day = drug_end_day + risk_window )


  # Make table in the db since it will be reused
  flog.trace("Computing intermediate drug durations table")
  # if( "tbl_dbi" %in% class( drug_durations ) ) drug_durations %>% explain()
  drug_durations <- drug_durations %>% compute()

  events_result <- drug_durations %>%
    transmute( person_id, concept_id,
               event_day = drug_start_day,
               event_flag = 1L,
               obs_period_id,
               observation_period_length )

  if( !lasting_risk )
    events_result <- events_result %>% union_all(
      drug_durations %>%
        transmute( person_id, concept_id,
                   event_day = drug_end_day,
                   event_flag = -1L,
                   obs_period_id,
                   observation_period_length )
    )

  # Return value
  union_all( events_result,
             condition_occurrence %>%
                 inner_join( observation_periods, by = "person_id" ) %>%
                 mutate( event_day = as.integer( condition_start_date - observation_period_start ) ) %>%
                 filter( condition_concept_id == event,
                         event_day >= 0,
                         event_day <= observation_period_length ) %>%
                 transmute( person_id, concept_id = condition_concept_id,
                            event_day,
                            event_flag = 1L,
                            obs_period_id,
                            observation_period_length ) ) %>%
    filter( event_day <= observation_period_length )

  # Code notes on the above:
  # We add 1 to the end date since the "event" is "no longer being exposed," this also folds in with how we turn this
  # into a feature table later (the corresponding interval becomes the first to have a 0 for that drug after a run of
  # 1s).
}
