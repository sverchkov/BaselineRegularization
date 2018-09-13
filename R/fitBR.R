#' learn a baseline regularization (BR) model
#'
#' learn a BR model from matrix and vector input
#' currently only support ridge penalty onver drug effects
#' currently only support one pair of lambda1 and lambda2
#' learning at a time
#'
#' @param Z design matrix that map tied baseline to
#' baseline on each interval
#'
#' @param interval_obs_period a vector indicating to which observation period/patient each interval belongs.
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
#' @param labmda2 strengh of fused lasso for baseline
#'
#' @param lambda3 strength of ridge regression for baseline
#'
#' @param ... currently unused
#'
#' @return a br model
#'
#' @export
#' @import futile.logger
#' @author Zhaobin Kuang
fitBR = function(Z,interval_obs_period,X,l,n,lambda1,lambda2,lambda3=0,...){

  max_outer_loop_iterations <- 1000
  max_inner_loop_iterations <- 1000
  outer_err_threshold <- 1e-6
  inner_err_threshold <- 1e-6

  # Interval grouping by baseline parameters, using which.max to get around "argument is not logical"
  interval_baseline <- apply( Z, 1, which.max )

  # Mapping of patients to baseline parameters
  baseline_obs_period <- interval_obs_period[ apply( Z, 2, which.max ) ]

  # total number of differences between adjacent basline parameters, which is
  # the number of distinct (tied) baseline parameters minus the number of patients
  n_baseline_parameters <- ncol( Z )
  n_baseline_diff <- n_baseline_parameters - length( unique( interval_obs_period ) )

  # initialize t and beta
  # Use MSCCS to initialize baseline parameters
  model_0 <- fitMSCCS(interval_obs_period,X,l,n,lambda=lambda1,threshold=1e-7)

  t <- msccs_t <- model_0$alpha[baseline_obs_period]
  beta <- msccs_beta <- model_0$beta

  flog.trace("Starting BR optimization loop...")

  # outer loop
  for( outer_loop_iteration in 1: max_outer_loop_iterations ){

    outer_err <- checkKKT4BR(Z,baseline_obs_period,X,l,n,t,beta,lambda1,lambda2,lambda3)
    flog.trace( "Outer Loop Iteration %4d Error: %20.8f", outer_loop_iteration, outer_err )
    if ( outer_err_threshold > outer_err ) break

    log_s <- as.numeric(Z%*%t+X%*%beta)
    w <- exp(log(l)+log_s)
    z <- as.numeric(log_s + n/(w) - 1)
    tTilde <- t
    betaTilde <- beta
    workingResponse <- z-Z%*%tTilde

    if ( outer_err == Inf ){
      flog.trace( "beta:", beta, capture = T )
      flog.trace( "Z*t:", Z%*%t, capture = T )
      flog.trace( "X*beta:", X%*%beta, capture = T )
      flog.trace( "log_s:", log_s, capture = T )
      flog.trace( "w:", w, capture = T )
      flog.trace( "workingResponse:", workingResponse, capture = T )
      flog.trace( "z", z, capture = T )
      flog.trace( "sum(l) = %s, sum(w) = %s", sum(l), sum(w) )
      #big_number <- .Machine$double.xmax
      #flog.warn( "Reducing infinities to %s", big_number )
      #workingResponse[ workingResponse == Inf ] <- big_number
    }

    omega <- w%*%Z + 2*lambda3

    # inner loop
    for( inner_loop_iteration in 1:max_inner_loop_iterations ){

      # beta step
      betaTilde <- getWls( y=workingResponse, X=X, w=w, lambda=lambda1*sum(l)/sum(w), thresh=1e-20)

      # t step
      nu <- ( t( w * ( z - X %*% betaTilde ) ) %*% Z ) / omega
      flog.trace( "nu?", nu, capture = T )

      tTilde = blockwiseWeightedFusedLassoSignalApproximator(indx = baseline_obs_period, y=nu, w=omega, lambda = lambda2 * n_baseline_diff )

      # check inner loop optimality
      workingResponse = z-Z%*%tTilde
      inner_err = checkKKT4BetaStep(y=workingResponse,X=X,w=w,beta=betaTilde,l=l,lambda=lambda1)

      flog.trace( "Inner Loop Iteration %4d Error: %16.8f", inner_loop_iteration, inner_err )

      flog.trace( "Beta~:", betaTilde, capture = T )

      if ( inner_err < inner_err_threshold ){
        beta <- betaTilde
        t <- tTilde
        break
      }
    }
  }
  flog.trace( "BR converged.")

  return( list( t=t, beta=beta, err=outer_err
           , msccs_t = msccs_t, msccs_beta = msccs_beta ) )
}






