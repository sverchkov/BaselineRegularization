#' Get condition events from a condition occurrence/era table and an observation period table
#'
#' Get condition events from a condition occurrence/era table and an observation period table:
#' takes the start date to be the event time, merges on person_id with observation periods, and filters to keep the event
#' within the observation period.
#'
#' Defaults work for the condition_occurrence table, need to specify date column for condition era
#' (`condition_era_start_date` according to OMOP CDM)
#'
#' @param condition the condition occurrence or condition era table
#' @param observation_period the observation period table
#' @param date_column (NSE evaluation) the column in the condition table that should be used as the condition event time,
#' `condition_start_date` by default.
#' @return a table of condition events
#'
#' @import dplyr
#' @importFrom rlang .data
#' @author Yuriy Sverchkov
getConditionEvents <- function ( condition, observation_period, date_column = !!br_symbol$condition_start_date ){

  date_sym <- rlang::enexpr( date_column )

  inner_join( condition, observation_period, by = "person_id" ) %>%
    mutate( event_day = as.integer( (!!date_sym) - !!br_symbol$observation_period_start_date ) ) %>%
    filter( !!br_symbol$event_day >= 0,
            !!br_symbol$event_day <= !!br_symbol$observation_period_length ) %>%
    transmute( concept_id = !!br_symbol$condition_concept_id,
               !!br_symbol$event_day,
               event_flag = 1L,
               !!br_symbol$observation_period_id,
               !!br_symbol$observation_period_length )
}
