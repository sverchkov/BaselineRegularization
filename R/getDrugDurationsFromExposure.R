#' Infer Drug Durations from the Drug Exposure Table
#'
#' Drug exposure durations are calculated as: exposure end date - exposure start date (if end date is specified) or
#' according to `days_supply` (if specified) or 1 day if neither is specified.
#'
#' @param drug_exposure A dataframe-like object corresponding to the OMOP CDM `drug_exposure` table, particularly, we use
#' the columns `person_id`, `drug_concept_id`, `drug_exposure_start_date`, `drug_exposure_end_date`, and `days_supply`
#' @param observation_period A dataframe-like object corresponding to the OMOP CDM `observation_period` table
#'
#' @author Yuriy Sverchkov
#' @importFrom rlang .data
#' @import dplyr
#' @import futile.logger
getDrugDurationsFromExposure <- function ( drug_exposure, observation_period )
{
  # Result
  inner_join( drug_exposure, observation_period, by = "person_id" ) %>%
    mutate( drug_start_day = as.integer( drug_exposure_start_date - observation_period_start_date ) ) %>%
    filter( drug_start_day >= 0, drug_start_day <= observation_period_length ) %>%
    transmute( observation_period_id,
               observation_period_length,
                     concept_id = drug_concept_id,
               drug_start_day,
                     drug_end_day =
                       1L + drug_start_day +
                       if_else( is.na( drug_exposure_end_date ),
                                if_else( is.na( days_supply ), 0L, days_supply ),
                                as.integer( drug_exposure_end_date - drug_exposure_start_date ) )
    )
}
