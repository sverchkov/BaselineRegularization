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
  # Check for infinite weights
  if ( any( inf_w <- is.infinite( w ) ) ){
    flog.warn( "Infinite weights found, dropping other weights and doing unweighted regression." )
    w <- sign( w[ inf_w ] )
    y <- y[ inf_w ]
    X <- X[ inf_w, ]
    l <- l[ inf_w ]

    flog.trace( "w", w, capture = T )
    flog.trace( "y", y, capture = T )
    flog.trace( "beta", beta, capture = T )
  }
  err = norm(t(X)%*%(w*(y-X%*%beta))-sum(l)*lambda*beta,type="2")
  return(err)
}


