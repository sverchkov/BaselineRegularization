#' check convergence of MSCCS
#'
#' check the convergence of MSCCS using KKT conditions
#'
#' @param Z fixed effect matrix
#' @param X drug exposure matrix
#' @param y response count vector
#' @param w weight vector that describe data repetition
#' @param lambda regularization strength, single lambda
#' @param alpha fixed effect coefficients
#' @param beta covariate coefficents
#'
#' @return 2-norm of a vector that specifies the degree of
#' optimality violation
#'
#' @author Zhaobin Kuang
checkKKT4MSCCS = function(Z,X,y,w,lambda,alpha,beta){

  # helper statistics
  r = exp(X %*% beta)
  yWeightSum = as.numeric(t(Z)%*%(w*y))
  rWeightSum = t(Z)%*%(w*r)

  # kkt
  # alpha
  alphaOpt = log(yWeightSum/rWeightSum)-alpha
  # beta
  likeStat = t(sweep(X,1,w,"*")) %*%
      Matrix(y-r/(Z %*% rWeightSum) *
      rep(yWeightSum,times=as.numeric(colSums(Z))))/sum(w)
  betaOpt = (likeStat/beta)-lambda

  vector <- rbind(alphaOpt,betaOpt)
  if ( any( is.infinite( vector ) ) ) {
    flog.trace("msccs optimality violation vector:", vector, capture = T )
    return ( Inf )
  }
  err = norm( vector,type="2")
  return(err)
}


