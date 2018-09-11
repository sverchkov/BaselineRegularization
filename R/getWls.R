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

  # multiplier <- sqrt( sum( w*( y-sum( w/sum(w)*y ) )^2 ) / ( sum(w) ) )
  multiplier <- sqrt( weighted.mean( ( y - weighted.mean( y, w ) )^2, w ) )

  if ( is.nan( multiplier ) ){
    flog.trace( "getWls lambda: %s, multiplier: %s, result: %s", lambda, multiplier, lambda*multiplier )
    flog.trace( "y", y, capture = T )
    flog.trace( "w", w, capture = T )
  }

  mdl = glmnet::glmnet(x=X,y=y,alpha=0,family="gaussian",weights=w,
               lambda=lambda*multiplier,
               standardize=FALSE,intercept=FALSE,thresh=thresh)
  beta = mdl$beta

  return(beta)
}
