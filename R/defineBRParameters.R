#' Define Baseline Regularization parameters
#'
#' Define parameters for Baseline Regularization. See KDD 2017 paper for algorithm details.
#'
#' @param lambda1 Main effect L1 regularization strength
#' @param lambda2 Fused lasso component regularization strength
#' @param lambda3 Baseline parameters L2 regularization strength
#' @param threshold Threshold for breaking out of inner loop early
#' @param maxOuterLoopIterations Max outer loop iterations
#' @param maxInnerLoopIterations Max inner loop iterations
#' @return a parameters object representing these parameter settings
#' @export
defineBRParameters <- function(
  lambda1 = 0,
  lambda2 = 0.5,
  lambda3 = 0.1,
  threshold = 1e-4,
  maxOuterLoopIterations = 60,
  maxInnerLoopIterations = 200 ) {
  list(
    lambda1 = lambda1,
    lambda2 = lambda2,
    lambda3 = lambda3,
    threshold = threshold,
    maxOuterLoopIterations = maxOuterLoopIterations,
    maxInnerLoopIterations = maxInnerLoopIterations
  )
}
