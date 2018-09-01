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
#' @import dplyr
getDrugDurationsFromExposure <- function ( drug_exposure, observation_period )
{
  # Result
  inner_join( drug_exposure, observation_period, by = "person_id" ) %>%
    mutate( drug_start_day = as.integer( !!br_symbol$drug_exposure_start_date - !!br_symbol$observation_period_start_date ) ) %>%
    filter( !!br_symbol$drug_start_day >= 0, !!br_symbol$drug_start_day <= !!br_symbol$observation_period_length ) %>%
    transmute( !!br_symbol$observation_period_id,
               !!br_symbol$observation_period_length,
               concept_id = !!br_symbol$drug_concept_id,
               !!br_symbol$drug_start_day,
               drug_end_day =
                       1L + !!br_symbol$drug_start_day +
                       if_else( is.na( !!br_symbol$drug_exposure_end_date ),
                                if_else( is.na( !!br_symbol$days_supply ), 0L, !!br_symbol$days_supply ),
                                as.integer( !!br_symbol$drug_exposure_end_date - !!br_symbol$drug_exposure_start_date ) )
    )
}
