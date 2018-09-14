#' compute weighted least square
#'
#' solve a weighted least square problem
#' it can be reugularized by a ridge penaly
#' currently only support one lambda at a time
#'
#' @param y response
#' @param X features
#' @param w weights
#' @param lambda ridge regularization strength
#' @param thresh defaut to be 1e-7, convergence criterion of
#' glmnet
#'
#' @return beta the regression coefficient
#'
#' @author Zhaobin Kuang
getWls = function(y,X,w,lambda,thresh=1e-7){

  # Check for infinite weights
  if ( any( inf_w <- is.infinite( w ) ) ){
    flog.warn( "Infinite weights found, dropping other weights and doing unweighted regression." )
    w <- sign( w[ inf_w ] )
    y <- y[ inf_w ]
    X <- X[ inf_w, ]
  }

  #multiplier <- 1
  multiplier <- sqrt( weighted.mean( ( y - weighted.mean( y, w ) )^2, w ) )

  if ( is.nan( multiplier ) ){
    flog.trace( "getWls lambda: %s, multiplier: %s, result: %s", lambda, multiplier, lambda*multiplier )
    flog.trace( "y", y, capture = T )
    flog.trace( "w", w, capture = T )
  }

  if ( any( is.infinite( y ) ) ) flog.trace( "y", y, capture = T )
  if ( any( is.infinite( w ) ) ) flog.trace( "y", w, capture = T )

  lambda_seq <- lambda * multiplier * c( 50, 20, 7, 2, 1 )

  mdl = glmnet::glmnet(x=X,y=y,alpha=0,family="gaussian",weights=w,
               lambda=lambda_seq,
               standardize=FALSE,intercept=FALSE,thresh=thresh)
  beta = mdl$beta[,length(lambda_seq)]

  return ( Matrix( beta ) )
}
