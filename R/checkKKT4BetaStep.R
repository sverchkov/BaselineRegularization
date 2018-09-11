#' check convergence for beta step
#'
#' check conveergence for beta step
#' used as an inner loop convergence criterion
#'
#' @param y response
#' @param X features
#' @param w weights
#' @param beta parameters learned
#' @param l interval length of each interval
#' @param lambda regularization strength
#'
#' @return 2-norm of a vector that specifies the degree of
#' optimality violation
#'
#' @author Zhaobin Kuang
checkKKT4BetaStep = function(y,X,w,beta,l,lambda){
  err = norm(t(X)%*%(w*(y-X%*%beta))-sum(l)*lambda*beta,type="2")
  return(err)
}


