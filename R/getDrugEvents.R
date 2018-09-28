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
      group_by( !!br_symbol$observation_period_id, !!br_symbol$observation_period_length, !!br_symbol$concept_id ) %>%
      summarize( event_day = min( !!br_symbol$drug_start_day, na.rm = T ) ) %>%
      ungroup() %>%
      mutate( event_flag = 1L )

  } else {

    # Apply risk window, if needed
    if ( risk_window > 0 ){

      # Ensure integer
      risk_window <- as.integer( risk_window )

      # Extend duration
      drug_durations <- drug_durations %>%
        mutate( drug_end_day = !!br_symbol$drug_end_day + risk_window )
    }

    # Merge overlapping drug durations, class-dependent implementations:
    flog.trace("Merging overlapping drug durations.")

    drug_durations <- ftry( {
      # This implementation works for something full-featured like postgres but will fail with SQLite
      drug_durations %>%
        group_by( !!br_symbol$observation_period_id, !!br_symbol$concept_id ) %>%
        arrange( !!br_symbol$observation_period_id, !!br_symbol$concept_id,
                 !!br_symbol$drug_start_day, !!br_symbol$drug_end_day ) %>%
        mutate( break_point =
                  ifelse( lag( !!br_symbol$drug_end_day, default = -1L ) < !!br_symbol$drug_start_day,
                          1L,
                          0L )
        ) %>%
        mutate( merge_group = cumsum( !!br_symbol$break_point ) ) %>%
        group_by( !!br_symbol$observation_period_id, !!br_symbol$observation_period_length,
                  !!br_symbol$concept_id, !!br_symbol$merge_group ) %>%
        summarize(
          merged_start_day = min( !!br_symbol$drug_start_day, na.rm = T ),
          merged_end_day = max( !!br_symbol$drug_end_day, na.rm = T ) ) %>%
        ungroup() %>%
        compute()
    }, error = function ( e ) {
      flog.info("Trying to recover by doing the calculation in R")
      drug_durations %>%
        group_by( !!br_symbol$observation_period_id, !!br_symbol$concept_id ) %>%
        arrange( !!br_symbol$observation_period_id, !!br_symbol$concept_id,
                 !!br_symbol$drug_start_day, !!br_symbol$drug_end_day ) %>%
        collect() %>%
        mutate( break_point =
                  ifelse( lag( !!br_symbol$drug_end_day, default = -1L ) < !!br_symbol$drug_start_day,
                          1L,
                          0L )
        ) %>%
        mutate( merge_group = cumsum( !!br_symbol$break_point ) ) %>%
        group_by( !!br_symbol$observation_period_id, !!br_symbol$observation_period_length,
                  !!br_symbol$concept_id, !!br_symbol$merge_group ) %>%
        summarize(
          merged_start_day = min( !!br_symbol$drug_start_day, na.rm = T ),
          merged_end_day = max( !!br_symbol$drug_end_day, na.rm = T ) ) %>%
        ungroup() %>%
        compute()
    } )

    union_all(
      drug_durations %>%
      transmute( !!br_symbol$concept_id,
                 event_day = !!br_symbol$merged_start_day,
                 event_flag = 1L,
                 !!br_symbol$observation_period_id,
                 !!br_symbol$observation_period_length ),
      drug_durations %>%
        transmute( !!br_symbol$concept_id,
                   event_day = !!br_symbol$merged_end_day,
                   event_flag = -1L,
                   !!br_symbol$observation_period_id,
                   !!br_symbol$observation_period_length )
    ) %>%
      filter( !!br_symbol$event_day <= !!br_symbol$observation_period_length )
  }
}
