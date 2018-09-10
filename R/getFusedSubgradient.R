#' get the subgradient of fused lasso
#'
#' get the subgraident of fused lasso given gradient and lambda
#'
#' @param grad gradient of the original fused lasso regularized problem
#'
#' @param lambda strength of fused lasso regularization, strictly positive
#'
#' @return fused lasso subgradient
#'
#' @author Zhaobin Kuang
getFusedSubgradient = function(grad,lambda){

  # the regularization strength should be strictly positive
  stopifnot(lambda>0)

  # corner case
  if(length(grad)==1) return(numeric())

  # usual case
  subgradient = rep(0,length(grad))
  for(i in 2:length(subgradient)){
    subgradient[i] = -grad[i-1]/lambda+subgradient[i-1]
  }

  return(subgradient[-1])
}
