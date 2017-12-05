#' Create data object
#'
#' Creates the data object for running Baseline Regularization
#' @param observation_period A dataframe-like view of the OMOP OBSERVATION_PERIOD table
#' @param drug_era A dataframe-like view of the OMOP DRUG_ERA table
#' @param condition_era A dataframe-like view of the OMOP CONDITION_ERA table
#' @param condition The condition_concept_id of the condition of interest
#' @param tying Parameter tying mode, "none", "interval", or "occurence" (default)
#' @param risk_window The number of days right after a drug era during which the patient is considered still under exposure
#' @param minimum_duration The number of days a patient must be under observation to be included in the analysis
#' @param independent_observation_periods Whether to treat distinct observation periods from one patient as distinct "patients"
#' @return An object containing the matrices X, Z, y
#' @export
prepareBRData <- function ( observation_period
                          , drug_era
                          , condition_era
                          , event
                          , tying = "occurence"
                          , risk_window = 0
                          , minimum_duration = 0
                          , independent_observation_periods = TRUE ){

  if ( ! independent_observation_periods )
    stop( "Current implementation only supports independent observation periods." )
  else {
    working_observation_periods <- filter( observation_period, observation_period_end_date - observation_period_end_date >= minimum_duration )
  }

  drug_era_events <- inner.join( working_observation_periods, drug_era, by = c( person_id = "person_id" ) )

  if ( risk_window > 0 ){
    # Risk window expansion
    drug_era_events <- mutate( drug_era_events, drug_era_end_date = drug_era_end_date + risk_window )
    # Check for each drug
  }

  drug_start_events <-
    drug_era_events %>%
    filter( drug_era_start_date >= observation_period_start_date, drug_era_start_date <= observation_period_end_date ) %>%
    select( event_era_id = drug_era_id
          , event_type = "drug start"
          , person_id
          , concept_id = drug_concept_id
          , event_date = drug_era_start_date
          , observation_period_id
          , observation_period_start_date
          , observation_period_end_date )

  drug_end_events <-
    drug_era_events %>%
    select( event_era_id = drug_era_id
          , event_type = "drug end"
          , person_id
          , concept_id = drug_concept_id
          , event_date = drug_era_end_date + 1 # Check that this is how you add a day
          , observation_period_id
          , observation_period_start_date
          , observation_period_end_date ) %>%
    filter( event_date >= observation_period_start_date, event_date <= observation_period_end_date )

  condition_events <-
    condition_era %>%
    select( event_era_id = condition_era_id
          , event_type = "condition start"
          , person_id
          , concept_id = condition_concept_id
          , event_date = condition_era_start_date
          , observation_period_id
          , observation_period_start_date
          , observation_period_end_date ) %>%
    filter( event_date >= observation_period_start_date, event_date <= observation_period_end_date )

  # bind rows
  events <- bind_rows( drug_start_events, drug_end_events, condition_events )

  # the distinct time breaks become the intervals. We sort them
  timebreaks <- events %>%
    distinct( event_date, observation_period_id ) %>%
    arrange( observation_period_if, event_date ) %>%
    mutate( interval = row_number() )

  # # # #

  stop("Not yet implemented")

  currConfig = subset(config,indx==x);
  dxIdWanted = currConfig$dxId;
  load(paste("../data/",parseConfig(x,currConfig),sep=""));
  nPatient = length(unique(interval$patientId));

  # drugId and dxId
  drugIdLong = sort(unique(truth$drugId)); # Drug names (as strings)
  dxIdLong = sort(unique(truth$dxId)); # Med names (as strings)
  drugId = 1:length(drugIdLong); # Drugs, as integers
  dxId = 1:length(dxIdLong); # Meds, as integers

  # general matrices
  obs = interval[,list(startAge = min(startAge),
                       endAge = max(endAge)), by="patientId"];
  l = interval$endAge - interval$startAge + 1; # Interval lengths
  n = interval$count; # Interval numbers?
  X = sparseMatrix(i = indxDrug$indx, j = indxDrug$drugId, x = 1,
                   dims=c(max(interval$indx),max(drugId))); # Drug occurences

  # one param per interval
  if(currConfig$tying == "interval"){
    Z = Diagonal(x=rep(1,nrow(interval)));
    segIndx = match(interval$patientId,
                    unique(interval$patientId));
  }else if(currConfig$tying == "occurence"){
    # one param per count
    indJ = as.numeric(interval$count>0);
    indJ[interval$intervalIndx==1]=1; indJ = cumsum(indJ);
    Z = sparseMatrix(i = 1:nrow(interval), j = indJ, x=1);
    segIndx = unique(data.table(patientId=interval$patientId,j=indJ));
    segIndx = match(segIndx$patientId,unique(segIndx$patientId));
  }

  # Return
  list(
    X = X,
    Z = Z,
    l = l,
    n = n,
    patients = patients
  )
}
