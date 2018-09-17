#' Define Baseline Regularization parameters
#'
#' Define parameters for Baseline Regularization. See KDD 2017 paper for algorithm details.
#'
#' @param lambda1 Main effect L1 regularization strength
#' @param lambda2 Fused lasso component regularization strength
#' @param lambda3 Baseline parameters L2 regularization strength
#' @param threshold Threshold for breaking out of inner loop early
#' @param max_outer_loop_iterations Max outer loop iterations
#' @param max_inner_loop_iterations Max inner loop iterations
#' @param save_trajectory Whether to save all intermediate beta and t vectors (uses up lots of memory!)
#' @return a parameters object representing these parameter settings
#' @export
defineBRParameters <- function(
  lambda1 = 0,
  lambda2 = 0.5,
  lambda3 = 0.1,
  threshold = 1e-4,
  max_outer_loop_iterations = 10000L,
  max_inner_loop_iterations = 100000L,
  save_trajectory = F ) {
  list(
    lambda1 = lambda1,
    lambda2 = lambda2,
    lambda3 = lambda3,
    threshold = threshold,
    max_outer_loop_iterations = max_outer_loop_iterations,
    max_inner_loop_iterations = max_inner_loop_iterations,
    save_trajectory = save_trajectory
  )
}
