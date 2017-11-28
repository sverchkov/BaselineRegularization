#' Define Baseline Regularization parameters
#'
#' Define parameters for Baseline Regularization
#'
#' @param lambda1 Main effect L1 regularization strength
#' @param lambda2 Fused lasso component regularization strength
#' @param lambda3 Baseline parameters L2 regularization strength
#' @return a parameters object representing these parameter settings
#' @export
defineBRParameters <- function(
  lambda1 = 0,
  lambda2,
  lambda3,
  threshold = 1e-4,
  maxOuterLoopIterations = 60,
  maxInnerLoopIterations = 200 ) {
  list(
    lambda1 = lambda1,
    lambda2 = lambda2,
    lambda3 = lambda3,
    threshold = threshold
  )
}
