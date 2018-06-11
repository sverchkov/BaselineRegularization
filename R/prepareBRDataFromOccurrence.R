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

  events <- getEventsFromOccurrence(
    con = con,
    drug_exposure = drug_exposure,
    visit_occurrence = visit_occurrence,
    event = event,
    risk_window = risk_window )

  flog.info("Handing over the event table")

  # Return
  prepareBRDataFromEvents( events, event, tying )
}
