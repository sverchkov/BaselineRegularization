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
#' @import Matrix
#' @author Zhaobin Kuang
getWls = function(y,X,w,lambda,thresh=1e-7){

  multiplier <- sqrt( stats::weighted.mean( ( y - stats::weighted.mean( y, w ) )^2, w ) )

  lambda_seq <- lambda * multiplier * c( 50, 20, 7, 2, 1 )

  tryCatch({
    mdl = glmnet::glmnet(x=X,y=y,alpha=0,family="gaussian",weights=w,
                 lambda=lambda_seq,
                 standardize=FALSE,intercept=FALSE,thresh=thresh)
  }, error = function ( e ) {
    flog.error( "glmnet crashed." )
    flog.debug( utils::str( e ) )
    flog.debug( "y = ", y, capture = T )
    flog.trace( "X = ", X, capture = T )
    flog.debug( "w = ", w, capture = T )
    flog.debug( "lambda = %s, multiplier = %s", lambda, multiplier )
    stop( e )
  } )

  beta = mdl$beta[,length(lambda_seq)]

  return ( Matrix( beta ) )
}
