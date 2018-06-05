#' Blockwise fused lasso signal approximator
#'
#' Blockwise fused lasso signal approximator adapted from the glmgen library / package by
#' Taylor Arnold, Ryan Tibshirani, and Veerun Sadhanala.
#'
#' @param indx block indeces
#' @param y y
#' @param lambda \eqn{\lambda}
#' @return \eqn{\beta}
#' @author Charles Kwong
#' @useDynLib BaselineRegularization, .registration = TRUE
blockwiseFusedLassoSignalApproximator = function(indx,y,lambda){
  # conversion
  indx = as.integer(indx);
  y = as.numeric(y);
  lambda = as.numeric(lambda);

  # call
  beta = .Call("bflsa",indx,y,lambda);
  return(beta);
}

