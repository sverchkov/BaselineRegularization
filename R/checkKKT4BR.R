#' check convergence of baseline regularization (BR)
#'
#' check the overall converge of BR
#' used as the outer loop stopping criterion
#'
#' @param Z design matrix for intervals
#' @param baseline_obs_period vector where index is the baseline parameter index and value is observation period index
#' @param X data matrix
#' @param l length of each interval
#' @param n number of dx occur at each interval
#' @param t baseline parameters
#' @param beta drug effect parameters
#' @param lambda1 strength of ridge regularization for beta
#' @param lambda2 strength of fused lasso regularization for baseline
#' @param lambda3 strength of ridge regression for baseline
#'
#' @return 2-norm of a vector that specifies the degree of
#' optimality violation
#'
#' @author Zhaobin Kuang
checkKKT4BR = function(interval_baseline_parameter,baseline_obs_period,X,l,n,t,beta,lambda1,lambda2,lambda3=0){

  # total number of differences between adjacent basline parameters
  nBaselineDiff = sum( duplicated( baseline_obs_period ) )
    # nrow(baseline) - nrow( distinct( baseline, patientId ) )

  # gradient
  log_s <- as.numeric( t[ interval_baseline_parameter ] + X%*%beta )
  w <- exp(log(l)+log_s)
  grad_t <- sumBy(w-n, interval_baseline_parameter) + lambda3*t
  grad_beta <- t(X)%*%(w-n)/sum(l) + lambda1*beta

  err <- do.call( c, lapply( unique( baseline_obs_period ), function( obs_period ){

    baseline_indx <- which( baseline_obs_period == obs_period )

    if ( length( baseline_indx ) > 1 ){ # more than one interval per observation period: fused err
      subgradient <- getFusedSubgradient( grad_t[baseline_indx], lambda2*nBaselineDiff )
      sgn <- sign( diff( t[baseline_indx] ) )
      checkL1Optimality( subgradient, sgn )
    } else if ( length( baseline_indx ) == 1 ) { # single interval per observation period: gradient err
      grad_t[ baseline_indx ]
    } else numeric( 0 ) # shouldn't even happen
  } ) )

  # err from beta
  if( any( grad_beta == Inf ) ) {
    flog.warn( "Infinite beta gradient computed." )
    return ( Inf )
  }

  err <- c(err, norm(grad_beta,type="2"))
  err <- norm(err,type="2")

  return(err)
}
