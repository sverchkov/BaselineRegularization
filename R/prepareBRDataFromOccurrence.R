#' Create data object for baseline regularization
#'
#' Creates the data object for running Baseline Regularization
#' @param con A DBI Connection to an OMOP CDM database.
#' @param drug_exposure Either the name of the DRUG_EXPOSURE table in the database specified by `con` or the
#' table itself in a dataframe-like R object
#' @param condition_occurence Either the name of the CONDITION_OCCURENCE table in the database specified by `con` or the
#' table itself in a dataframe-like R object
#' @param visit_occurence Either the name of the VISIT_OCCURENCE table in the database specified by `con` or the
#' table itself in a dataframe-like R object.
#' A patient will be considered observed from their first to their last visit.
#' If this parameter is NULL then the patient will be considered from their first to their last drug exposure and/or
#' condition occurence
#' @param event The condition_concept_id of the event of interest
#' @param tying Parameter tying mode, "interval", or "occurence" (default)
#' @param risk_window The number of days right after a drug exposure during which the patient is considered still under
#' exposure.
#' @param minimum_duration The number of days a patient must be under observation to be included in the analysis.
#' @param independent_observation_periods Whether to treat distinct observation periods from one patient as distinct
#' "patients." Default value and only current accepted value is TRUE.
#' @return An object containing the matrices X, Z, y
#' @import futile.logger
#' @import tidyr
#' @import dplyr
#' @export
prepareBRDataFromOccurrence <- function( con = NULL
                                       , drug_exposure = "DRUG_EXPOSURE"
                                       , condition_occurrence = "CONDITION_OCCURRENCE"
                                       , visit_occurrence = "VISIT_OCCURRENCE"
                                       , event
                                       , tying = "occurrence"
                                       , risk_window = 0
                                       , minimum_duration = 0 ){

  if ( !( tying %in% c("occurrence", "interval" ) ) ) {
    stop( flog.fatal( "Invalid 'tying' parameter (%s) supplied.", tying ) )
  }

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

  events <-
    drug_durations %>%
      transmute( person_id, concept_id,
                 event_date = drug_exposure_start_date,
                 event_flag = 1L ) %>%
    union_all( drug_durations %>%
                 transmute( person_id, concept_id,
                            event_date = drug_exposure_end_date + 1L,
                            event_flag = -1L ) ) %>%
    union_all( condition_occurrence %>%
                 inner_join( valid_persons, by = "person_id" ) %>%
                 filter( condition_concept_id == event ) %>%
                 transmute( person_id, concept_id = condition_concept_id,
                            event_date = condition_start_date,
                            event_flag = 1L ) ) %>%
    inner_join( observation_periods, by = "person_id" )

  # Code notes on the above:
  # We add 1 to the end date since the "event" is "no longer being exposed," this also folds in with how we turn this
  # into a feature table later (the corresponding interval becomes the first to have a 0 for that drug after a run of
  # 1s).

  # Return
  prepareBRDataFromEvents( events, event, tying )
}
