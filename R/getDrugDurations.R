#' Make events table from occurrence information (internal function)
#'
#' Make events table for [prepareBRDataFromEvents] from occurrence tables (rather than era tables).
#'
#' Drug exposure durations are calculated as: exposure end date - exposure start date (if end date is specified) or
#' according to `days_supply` (if specified) or 1 day if neither is specified, plus `risk_window`.
#'
#' @param drug_exposure A dataframe-like object corresponding to the OMOP CDM `drug_exposure` table
#' @param condition_occurrence A dataframe-like object corresponding to the OMOP CDM `condition_occurrence` table
#' @param observation_period A dataframe-like object corresponding to the OMOP CDM `observation_period` table
#' @param event The event of interest
#' @param risk_window See note on drug exposure duration calculation
#' @param minimum_duration The minimum duration that a patient needs to be under observation to be included in the
#' analysis.
#' @param drug_sym A [name] representing the column to use for drug ids, drug_concept_id by default.
#' @param condition_sym A [name] representing the column to use for condition ids, condition_concept_id by default.
#' @param person_sym A [name] representing the column to use for person ids, person_id by default.
#'
#' @author Yuriy Sverchkov
#' @import dplyr
#' @import futile.logger
drugEventsFromExposure( drug_exposure,
                        observation_period,
                        risk_window,
                        person_sym,
                        drug_sym,
                        drug_start_sym = drug_start_sym,
                        drug_end_sym = drug_end_sym,
                        observation_period_sym = observation_period_sym,
                        observation_period_start_date_sym = observation_period_start_date_sym,
                        observation_period_end_date_sym = observation_period_end_date_sym )
{
  # Ensure correct types
  lasting_risk <- Inf == risk_window
  if ( !lasting_risk ) risk_window <- as.integer( risk_window )

  # Result
  drug_exposure %>%
    inner_join( observation_periods, by = deparse( person_sym ) ) %>%
    mutate( drug_start_day = as.integer( !!drug_start_sym - !!observation_period_start_date_sym ) ) %>%
    filter( drug_start_day >= 0, drug_start_day <= observation_period_length ) %>%
    transmute( !!person_sym,
               !!observation_period_sym,
               observation_period_length,
               concept_id = !!drug_sym,
               drug_start_day,
               drug_end_day =
                 1L + drug_start_day +
                 if_else( is.na( !!drug_end_sym ),
                          if_else( is.na( !!supply_sym ), 0L, !!supply_sym ),
                          as.integer( !!drug_end_sym - !!drug_start_sym ) )
    )

}

  # Return value
  union_all( events_result,
             condition_occurrence %>%
                 inner_join( observation_periods, by = deparse( person_sym ) ) %>%
                 mutate( event_day = as.integer( !!condition_start_sym - !!observation_period_start_date_sym ) ) %>%
                 filter( condition_concept_id == event,
                         event_day >= 0,
                         event_day <= observation_period_length ) %>%
                 transmute( !!person_sym, concept_id = !!condition_sym,
                            event_day,
                            event_flag = 1L,
                            observation_period_id,
                            observation_period_length ) ) %>%
    filter( event_day <= observation_period_length )

  # Code notes on the above:
  # We add 1 to the end date since the "event" is "no longer being exposed," this also folds in with how we turn this
  # into a feature table later (the corresponding interval becomes the first to have a 0 for that drug after a run of
  # 1s).
}
