#' Get drug events from drug durations
#'
#' Takes a drug durations table, extends
#'
#' @import futile.logger
#' @import dplyr
#' @importFrom rlang .data
#' @author Yuriy Sverchkov
getDrugEvents <- function( drug_durations, risk_window = 0 ) {

  # Check for lasting risk
  lasting_risk <- ( risk_window == Inf )

  if ( lasting_risk ){ # Lasting risk means we keep only the first drug occurrence

    drug_durations %>%
      group_by( observation_period_id, observation_period_length, concept_id ) %>%
      summarize( event_day = min( drug_start_day ) ) %>%
      ungroup() %>%
      mutate( event_flag = 1L )

  } else {
    flog.fatal("Code for non-lasting risk window (!= Inf) currently broken.")
    # Apply risk window, if needed
    if ( risk_window > 0 ){

      # Ensure integer
      risk_window <- as.integer( risk_window )

      # Extend duration
      drug_durations <- drug_durations %>%
        mutate( drug_end_day = drug_end_day + risk_window )
    }

    # Merge overlapping drug durations
    drug_durations <- drug_durations %>%
      group_by( observation_period_id, concept_id ) %>%
      arrange( )


    # Make table in the db since it will be reused
    flog.trace("Computing intermediate drug durations table")
    drug_durations <- drug_durations %>% compute()

    union_all(
      drug_durations %>%
      transmute( concept_id,
                 event_day = drug_start_day,
                 event_flag = 1L,
                 observation_period_id,
                 observation_period_length ),
      drug_durations %>%
        transmute( concept_id,
                   event_day = drug_end_day,
                   event_flag = -1L,
                   observation_period_id,
                   observation_period_length )
    )
  }
}
