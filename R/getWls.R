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
#' @export
#'
getWls = function(y,X,w,lambda,thresh=1e-7){

  mdl = glmnet(x=X,y=y,alpha=0,family="gaussian",weights=w,
               lambda=lambda*sqrt(sum(w*(y-sum(w/sum(w)*y))^2)/(sum(w))),
               standardize=FALSE,intercept=FALSE,thresh=thresh)
  beta = mdl$beta

  return(beta)
}
