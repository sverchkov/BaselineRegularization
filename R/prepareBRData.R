#' Create data object
#'
#' Creates the data object for running Baseline Regularization
#' @param data A dplyr tibble that contains the columns [...]
#' @param event Event of interest
#' @param tying Parameter tying mode, "none", "interval", or "occurence" (default)
#' @param risk_window The number of days right after a drug era during which the patient is considered still under exposure
#' @param minimum_duration The number of days a patient must be under observation to be included in the analysis
#' @return An object containing the matrices X, Z, y
#' @export
prepareBRData <- function ( data, event, tying = "occurence", risk_window = 0, minimum_duration = 0 ){

  pre_table <- spread( data = events, key = concept_id, value = 1, fill = 0, sep = "_" ) 
  
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
    X = X
  )
}
