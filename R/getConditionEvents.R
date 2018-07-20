#'
#' @import dplyr
#' @importFrom rlang .data
#' @author Yuriy Sverchkov
getConditionEvents <- function ( condition_occurrence, observation_periods ){
  inner_join( condition_occurrence, observation_periods, by = "person_id" ) %>%
    mutate( event_day = as.integer( condition_start_date - observation_period_start_date ) ) %>%
    filter( event_day >= 0,
            event_day <= observation_period_length ) %>%
    transmute( concept_id = condition_concept_id,
               event_day,
               event_flag = 1L,
               observation_period_id,
               observation_period_length )
}
