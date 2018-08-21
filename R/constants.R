#' @author Yuriy Sverchkov
#' Constants used throughout (mostly symbol defaults)

br_symbol <- {
  nam <- c(
    "Beta",
    "ancestor_concept_id",
    "break_point",
    "concept_id",
    "concept_name",
    "condition_concept_id",
    "condition_era_start_date",
    "condition_start_date",
    "days_supply",
    "descendant_concept_id",
    "drug_concept_id",
    "drug_end_day",
    "drug_era_end_date",
    "drug_era_start_date",
    "drug_exposure_end_date",
    "drug_exposure_start_date",
    "drug_start_day",
    "event_day",
    "event_time",
    "interval_number",
    "lead_interval",
    "merge_group",
    "merged_start_day",
    "merged_end_day",
    "number_of_intervals",
    "observation_period_end_date",
    "observation_period_id",
    "observation_period_length",
    "observation_period_start_date",
    "person_id" )
  setNames( lapply( nam, as.symbol ), nam )
}
