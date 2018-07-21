#' Get drug events from drug durations
#'
#' Takes a drug durations table, extends the durations by risk_window, merges overlapping durations, and retuens a
#' table of drug start and end events
#'
#' @param drug_durations Drug durations table as returned by [getDrugDurationsFromExposure].
#' @param risk_window Risk window in days to be added to durations, with [Inf] indicating lasting risk.
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

    # Apply risk window, if needed
    if ( risk_window > 0 ){

      # Ensure integer
      risk_window <- as.integer( risk_window )

      # Extend duration
      drug_durations <- drug_durations %>%
        mutate( drug_end_day = drug_end_day + risk_window )
    }

    # Merge overlapping drug durations, class-dependent implementations:
    flog.trace("Merging overlapping drug durations.")

    drug_durations <- if ( "tbl_dbi" %in% class( drug_durations ) ){
      # This implementation works for something full-featured like postgres but will fail with SQLite
      drug_durations %>%
        group_by( observation_period_id, concept_id ) %>%
        arrange( observation_period_id, concept_id, drug_start_day ) %>%
        mutate( merge_point =
                  ifelse( lead( drug_start_day, default = -1L ) < drug_end_day,
                          1L,
                          0L )
        ) %>%
        mutate( merge_group = cumsum( merge_point ) ) %>%
        group_by( observation_period_id, observation_period_length, concept_id, merge_group ) %>%
        summarize( merged_start_day = min( drug_start_day ), merged_end_day = max( drug_end_day ) ) %>%
        ungroup() %>%
        compute()

    } else {
      # This implementation is more intuitive to understand, but we should test whether it's faster than the one above
      # for data frames
      drug_durations %>%
        group_by( observation_period_id, concept_id ) %>%
        arrange( observation_period_id, concept_id, drug_start_day ) %>%
        mutate( merged_start_day =
                  ifelse( lag( drug_end_day, default = -1L ) > drug_start_day,
                          lag( merged_start_day ),
                          drug_start_day )
                ) %>%
        group_by( observation_period_id, observation_period_length, concept_id, merged_start_day ) %>%
        summarize( merged_end_day = max( drug_end_day ) ) %>%
        ungroup()
    }

    union_all(
      drug_durations %>%
      transmute( concept_id,
                 event_day = merged_start_day,
                 event_flag = 1L,
                 observation_period_id,
                 observation_period_length ),
      drug_durations %>%
        transmute( concept_id,
                   event_day = merged_end_day,
                   event_flag = -1L,
                   observation_period_id,
                   observation_period_length )
    )
  }
}
