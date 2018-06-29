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
                                    , time_match = "_date$" )
{
  tables <- list(...) # Should we consider changing this to rlang::list2 ? Or something else?

  n <- length( tables )

  if ( length( tables ) < 1 ) return ( NULL )

  ptid <- rlang::sym( patient_id )

  processed_tables <- Map( function ( tab ) {
    all_cols <- colnames( tab )
    date_cols <- all_cols[ grep( time_match, all_cols, ignore.case = T ) ]

    flog.trace("Columns:", all_cols, capture = T )
    flog.trace("Date columns:", date_cols, capture = T)

    # Account for the unlikely
    if( "event_time" %in% date_cols )
      date_cols <- c( "event_time", date_cols[ -which(date_cols == "event_time") ] )

    result <- tab %>% select( `!!`( ptid ), event_time = `!!`(rlang::sym( date_cols[1] )) )

    for ( column in date_cols[-1] ){
      result <- union_all( result, tab %>% select( `!!`( ptid ), event_time = `!!`(rlang::sym( column )) ) )
    }

    result
  }, tables )

  result <- processed_tables[[1]]
  for ( tab in processed_tables[-1] ) {
    result <- union_all( result, tab )
  }

  result %>% group_by( `!!`(ptid) ) %>%
    summarize(
      observation_period_start = min( event_time, na.rm = T ),
      observation_period_end = max( event_time, na.rm = T ) ) %>%
    ungroup()
}
