#' learn a baseline regularization (BR) model
#'
#' learn a BR model from matrix and vector input
#' currently only support ridge penalty onver drug effects
#' currently only support one pair of lambda1 and lambda2
#' learning at a time
#'
#' @param interval_baseline_parameter An array where each position corresponds to an interval and each value
#' corresponds to the index of a baseline parameter
#'
#' @param baseline_parameter_obs_period An array where each position corresponds to a baseline parameter and each
#' value corresponds to the index of an observation period
#'
#' @param X data matirx, each row represent data from
#' one interval, each column represent drug exposure info
#'
#' @param l length of each interval
#'
#' @param n number of condition occurrences on each interval
#'
#' @param lambda1 strength of ridge regression for beta
#'
#' @param lambda2 strengh of fused lasso for baseline
#'
#' @param lambda3 strength of ridge regression for baseline
#'
#' @param ... currently unused
#'
#' @return a br model
#'
#' @export
#' @import Matrix
#' @import futile.logger
#' @author Zhaobin Kuang
#' @author Yuriy Sverchkov
fitBR <- function( interval_baseline_parameter, baseline_parameter_obs_period, X, l, n, lambda1, lambda2, lambda3, ... ){

  # Initialize result
  result <- list()

  # Set up control
  extra_args <- list( ... )

  if ( save_trajectory <- valueOrDefault( extra_args$save_trajectory ) ){
    result$trajectory <- list()
  }

  max_outer_loop_iterations <- valueOrDefault( extra_args$max_outer_loop_iterations, 10000L )
  max_inner_loop_iterations <- 1000000L
  outer_err_threshold <- 1e-6
  inner_err_threshold <- 1e-6
  plateau_steps <- 10L # Number of iterations without change to trigger early termination

  # Note: interval_baseline_parameter is a more compact representation of "Z" from the paper.
  # For a vector v, up to transposition,
  # v %*% Z = t(Z) %*% v = sumBy( v, interval_baseline_parameter )
  # Z %*% v = v %* t(Z) = v[ interval_baseline_parameter ]

  # total number of adjacent baseline parameters within observation periods
  n_baseline_diff <- sum( duplicated( baseline_parameter_obs_period ) )

  # initialize t and beta
  # Use MSCCS to initialize baseline parameters
  result$msccs_model <- fitMSCCS( baseline_parameter_obs_period[ interval_baseline_parameter ], X, l, n, lambda=lambda1, threshold=1e-7)

  t <- result$msccs_model$alpha[ baseline_parameter_obs_period ]
  beta <- result$msccs_model$beta

  old_t <- 0
  old_beta <- 0
  plateau_counter <- 0L

  flog.debug("Starting BR optimization loop...")

  termination_reason <- c( "iter" = "reached maximum iterations." )

  # outer loop
  for ( outer_loop_iteration in 1: max_outer_loop_iterations ) {

    outer_err <- checkKKT4BR( interval_baseline_parameter, baseline_parameter_obs_period, X, l, n, t, beta, lambda1, lambda2, lambda3 )
    flog.debug( "Outer Loop Iteration %4d Error: %20.8f", outer_loop_iteration, outer_err )
    if ( outer_err_threshold > outer_err ){
      termination_reason <- c( "kkt" = "converged." )
      break
    }

    # Plateau termination
    if ( isTRUE( all.equal( old_t, t ) ) && isTRUE( all.equal( old_beta, beta ) ) ){
      if ( plateau_counter < plateau_steps ) plateau_counter <- 1L + plateau_counter
      else {
        termination_reason <- c( "plateau" = "optimization plateaued." )
        break
      }
    } else {
      old_t <- t
      old_beta <- beta
      plateau_counter <- 0L
    }

    log_s <- as.numeric( t[ interval_baseline_parameter ] + X %*% beta )
    w <- exp( log(l) + log_s ) # Weight vector. of length # of intervals
    w <- pmax( w, .Machine$double.xmin ) # Avoid crash due to 0 weights
    z <- as.numeric( log_s + n / (w) - 1 )
    tTilde <- t
    betaTilde <- beta

    omega <- sumBy( w, interval_baseline_parameter ) + 2*lambda3

    # inner loop
    for( inner_loop_iteration in 1:max_inner_loop_iterations ){

      workingResponse <- z - tTilde[ interval_baseline_parameter ]

      if ( inner_loop_iteration > 1 ) {
        # check inner loop optimality
        inner_err <- checkKKT4BetaStep(y=workingResponse,X=X,w=w,beta=betaTilde,l=l,lambda=lambda1)

        flog.trace( "Inner Loop Iteration %4d Error: %16.8f", inner_loop_iteration, inner_err )

        if ( inner_err < inner_err_threshold ){
          beta <- betaTilde
          t <- tTilde
          break
        }
      }

      # beta step
      betaTilde <- getWls( y=workingResponse, X=X, w=w, lambda=lambda1*sum(l)/sum(w), thresh=1e-20)
      # ^ lambda is scaled by total interval length divided (why?) by the sum of weights

      # t step
      nu <- sumBy( w * ( z - X %*% betaTilde ), interval_baseline_parameter ) / omega

      tTilde <- blockwiseWeightedFusedLassoSignalApproximator(
        indx = baseline_parameter_obs_period,
        y=nu,
        w=omega,
        lambda = lambda2 * n_baseline_diff ) # lambda is scaled by # of baseline parameter adjacencies
    }

    if ( save_trajectory ){
      result$trajectory[[outer_loop_iteration]] <- list( t = t, beta = beta )
    }
  }

  flog.debug( "BR %s", termination_reason )

  result$t <- t
  result$beta <- beta
  result$err <- outer_err

  return ( result )
}






