#' check whether l1 optimality
#'
#' check wehther a subgradient of a l1 penalty is optimal
#'
#' @param subgradient subgraident of l1-norm
#' @param sgn sign of the corresponding sub gradient
#'
#' @return 2-norm whose value indicate optimality violation
#'
#' @export
#'
checkL1Optimality = function(subgradient,sgn){
  if(length(subgradient)==1) return(numeric())
  res = c(subgradient[sgn>0]-1,subgradient[sgn<0]+1,pmax(abs(subgradient[sgn==0]),1)-1)
  err = norm(as.numeric(res),type="2")
  return(err)
}

