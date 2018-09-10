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
#' @import dplyr
#' @author Zhaobin Kuang
checkKKT4BR = function(Z,baseline_obs_period,X,l,n,t,beta,lambda1,lambda2,lambda3=0){

  # Table for grouping things later on
  baseline = tibble::tibble( obs_period = baseline_obs_period ) %>%
    mutate( baseline_indx = row_number() )

  # total number of differences between adjacent basline parameters
  nBaselineDiff = sum( duplicated( baseline_obs_period ) )
    # nrow(baseline) - nrow( distinct( baseline, patientId ) )

  # gradient
  log_s <- as.numeric(Z%*%t+X%*%beta)
  w <- exp(log(l)+log_s)
  grad_t <- t(Z)%*%(w-n) + lambda3*t
  grad_beta <- t(X)%*%(w-n)/sum(l) + lambda1*beta

  baseline <- baseline %>% mutate( t = t, grad_t = as.numeric(grad_t) )

  err <- do.call( c, lapply( unique( baseline_obs_period ), function( obs_period ){

    baseline_indx <- which( baseline_obs_period == obs_period )

    if ( length( baseline_indx ) > 1 ){ # more than one interval per observation period: fused err
      subgradient <- getFusedSubgradient( grad_t[baseline_indx], lambda2*nBaselineDiff )
      sgn <- sign( diff( t[baseline_indx] ) )
      checkL1Optimality( subgradient, sign )
    } else if ( length( baseline_indx ) == 1 ) { # single interval per observation period: gradient err
      grad_t[ baseline_indx ]
    } else numeric( 0 ) # shouldn't even happen
  } ) )

  # err from beta
  err <- c(err, norm(grad_beta,type="2"))
  err <- norm(err,type="2")

  return(err)



}
