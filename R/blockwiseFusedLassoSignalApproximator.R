#' Blockwise fused lasso signal approximator
#'
#' @param indx
#' @param y
#' @param lambda /lambda
#' @return /beta
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

