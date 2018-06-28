#' Construct an observation period table from a set of tables
#'
#' Takes one or more tables and returns a table of 'patient' observation periods basd on the first and last dates
#' that are associated with the patients
#'
#' @param ... One or more tables
#' @param patient_id specifies the column that corresponds to the patient ID (each observation period has only one of these)
#' @param time_match to match to columns that correspond to vent times
#' @return a table with three columns: patient_id, observation_period_start, observation_period_end
#' @import dplyr
inferObservationPeriods <- function ( ...
                                    , patient_id = "patient_id"
                                    , time_match = "_date" )
{
  tables <- list(...) # Should we consider changing this to rlang::list2 ? Or something else?

  n <- length( tables )

  if ( length( tables ) < 1 ) return ( NULL )

  ptid <- rlang::sym( patient_id )

  processed_tables <- Map( function ( tab ) {
    all_cols <- colnames( tab )
    date_cols <- all_cols[ grep( tolower( time_match ), tolower( all_cols ) ) ]
    min_cols <- paste0( date_cols, "_min" )
    max_cols <- paste0( date_cols, "_max" )

    flog.trace("Columns:", all_cols, capture = T )
    flog.trace("Date columns:", date_cols, capture = T)

    tab <- tab %>%
      mutate_at( vars( date_cols ), as.Date ) %>%
      group_by( `!!`( ptid ) ) %>%
      summarize_at( vars( date_cols ), funs( min, max, .args = list(  na.rm = TRUE ) ) ) %>%
      ungroup() %>%
      mutate(
        event_min = `!!`(rlang::sym(min_cols[1])),
        event_max = `!!`(rlang::sym(max_cols[1])) )

    for ( date_col in max_cols[-1] ){
      tab <- tab %>%
        mutate( event_max = ifelse( is.na( event_max ),
                                    `!!`(rlang::sym(date_col)),
                                    ifelse( is.na( `!!`(rlang::sym(date_col)) ),
                                                   event_max,
                                                   ifelse( `!!`(rlang::sym(date_col)) > event_max,
                                                           `!!`(rlang::sym(date_col)),
                                                           event_max
                                                           )
                                            )
                                    ) )
    }

    for ( date_col in min_cols[-1] ){
      tab <- tab %>%
        mutate( event_min = ifelse( is.na( event_min ),
                                    `!!`(rlang::sym(date_col)),
                                    ifelse( is.na( `!!`(rlang::sym(date_col)) ),
                                                   event_min,
                                                   ifelse( `!!`(rlang::sym(date_col)) < event_min,
                                                           `!!`(rlang::sym(date_col)),
                                                           event_min
                                                           )
                                            )
                ) )
    }

    tab
  }, tables )

  result <- processed_tables[[1]]
  for ( tab in processed_tables[-1] ) {
    result <- union_all( result, tab )
  }

  result %>% group_by( `!!`(ptid) ) %>%
    summarize( observation_period_start = min( event_min ), observation_period_end = max( event_max ) ) %>%
    ungroup()
}
