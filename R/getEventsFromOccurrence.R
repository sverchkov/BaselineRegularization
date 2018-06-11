#' Make events table from occurrence information
#'
#' @import dplyr
#' @import futile.logger
getEventsFromOccurrence <- function( con
                                   , drug_exposure
                                   , condition_occurrence
                                   , visit_occurrence
                                   , event
                                   , risk_window
                                   )
{
  if ( !is.null( con ) ){

    if ( isSingleString( drug_exposure ) ){
      flog.info( "Using drug exposure table '%s' from the database.", drug_exposure )
      drug_exposure <- getDBTable( con, drug_exposure )
    }

    if ( isSingleString( condition_occurrence ) ){
      flog.info( "Using condition occurrence table '%s' from the database.", condition_occurrence )
      condition_occurrence <- getDBTable( con, condition_occurrence )
    }

    if ( isSingleString( visit_occurrence ) ){
      flog.info( "Using visit occurrence table '%s' from the database.", visit_occurrence )
      visit_occurrence <- getDBTable( con, visit_occurrence )
    }
  }

  # Derive observation period per person
  observation_times <-
    union_all( drug_exposure %>% select( person_id, date = drug_exposure_start_date ),
               drug_exposure %>%
                 filter( !is.na( drug_exposure_end_date ) ) %>%
                 transmute( person_id, date = drug_exposure_end_date + 1L ) ) %>%
    union_all( condition_occurrence %>% select( person_id, date = condition_start_date ) ) %>%
    union_all( condition_occurrence %>%
                 filter( !is.na( condition_end_date ) ) %>%
                 transmute( person_id, date = condition_end_date + 1L ) )

  # If there are visit events for patients, include them for the sake of the timeline
  if ( !is.null( visit_occurrence ) ){
    observation_times <- observation_times %>%
      union_all( visit_occurrence %>% select( person_id, date = visit_start_date ) ) %>%
      union_all( visit_occurrence %>%
                   filter( !is.na( visit_end_date ) ) %>%
                   transmute( person_id, date = visit_end_date + 1L ) )
  }

  observation_periods <- observation_times %>%
    group_by( person_id ) %>%
    summarize(
      observation_period_start_date = min( date, na.rm = TRUE ),
      observation_period_end_date = max( date, na.rm = TRUE ) ) %>%
    ungroup() %>%
    filter( observation_period_end_date - observation_period_start_date > as.integer( minimum_duration ) ) %>%
    mutate( obs_period = dense_rank( person_id ) )

  valid_persons <- observation_periods %>% select( person_id )


  # Drug event window expansion
  drug_durations <- drug_exposure %>%
    inner_join( valid_persons, by = "person_id" ) %>%
    transmute( person_id
               , concept_id = drug_concept_id
               , drug_exposure_start_date
               , drug_exposure_end_date = ifelse( is.na( drug_exposure_end_date )
                                                  , drug_exposure_start_date
                                                  , drug_exposure_end_date )
    )

  if ( risk_window > 0 )
    drug_durations <- drug_durations %>%
    mutate( drug_exposure_end_date = drug_exposure_end_date + as.integer( risk_window ) )

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