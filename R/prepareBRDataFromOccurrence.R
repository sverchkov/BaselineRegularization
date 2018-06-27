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

  flog.trace("Preparing the event table")

  events <- getEventsFromOccurrence(
    drug_exposure = drug_exposure,
    condition_occurrence = condition_occurrence,
    visit_occurrence = visit_occurrence,
    event = event,
    risk_window = risk_window,
    minimum_duration = minimum_duration )

  flog.trace("Handing over the event table")

  if( "SQLiteConnection" == class( con ) ||
      ( ( "tbl_dbi" %in% class( events ) ) &&
        ( "src_dbi" %in% class( events$src ) ) &&
        ( "SQLiteConnection" == class( events$src$con ) ) ) ){
    flog.warn( "SQLite detected, will do more work in memory" )

    # Return
    prepareBRDataFromEvents2( events, event, tying )
  } else {

    # Return
    prepareBRDataFromEvents( events, event, tying )
  }
}
