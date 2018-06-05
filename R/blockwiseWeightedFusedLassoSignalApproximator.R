#' Blockwise weighted fused lasso signal approximator
#'
#' Blockwise weighted fused lasso signal approximator adapted from the glmgen library / package by
#' Taylor Arnold, Ryan Tibshirani, and Veerun Sadhanala.
#'
#' @param indx block indeces
#' @param y y
#' @param w weights
#' @param lambda lambda
#' @return beta
#' @author Charles Kwong
#' @useDynLib BaselineRegularization, .registration = TRUE
blockwiseWeightedFusedLassoSignalApproximator = function(indx,y,w,lambda){

  # conversion
  indx = as.integer(indx);
  y = as.numeric(y);
  w = as.numeric(w);
  lambda = as.numeric(lambda);

  # call
  beta = .Call("bwflsa",indx,y,w,lambda);
  return(beta);
}
