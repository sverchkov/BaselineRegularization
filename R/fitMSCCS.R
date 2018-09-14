#' run MSCCS as an initialization for BR
#'
#' run MSCCS given matrix and vector input data from BR
#' currently only support ridge regression over drug effects
#' currently only support single lambda
#' this is used to initialze BR
#'
#' @param obs_groups interval groups (observation periods)
#' @param X drug exposure matrix
#' @param l interval length
#' @param n number of response condition occurrences in an interval
#' @param lambda regularization strength, single lambda
#' @param threshold same as in glmnet
#'
#' @return an msccs model (list) that includes alpha, beta,
#' lambda, and err
#'
#' @export
#' @import Matrix
#' @author Zhaobin Kuang
fitMSCCS = function(obs_group,X,l,n,lambda,threshold=1e-7){

  # single lambda
  stopifnot(length(lambda)==1)

  # create Z X y w compatible to msccs by listing
  # first no occurrence of dx then occurrence of dx

  w <- c(l-n,rep(1,sum(n)))

  #-------c( each i, each i w/ occ # of times it occ'd
  indx <- c( 1:length(n), rep( which( n>0 ), times = n[n>0] ) )

  y <- c(rep(0,length(l)),rep(1,sum(n))) # 0's and 1's to match the above

  # Remove w=0 rows
  keep = w > 0
  indx <- indx[keep]
  w <- w[keep]
  y <- y[keep]

  # Make X to match y
  X <- Matrix( X[indx,] ) # ensure this is a matrix in case there's only obs period.
  Z <- sparseMatrix(i = 1:length(indx), j = obs_group[indx] )

  # learn MSCCS
  isPenalized <- c(rep(0,ncol(Z)),rep(1,ncol(X))) # We only want X features penalized

  mdl = glmnet::glmnet( x = cbind(Z,X), y = y, family = "poisson", weights = w,
    lambda = lambda*sum(isPenalized)/length(isPenalized),
    standardize = FALSE, intercept = FALSE, penalty.factor=isPenalized,
    alpha = 0, thresh = threshold)

  # extract coefficient
  alpha <- Matrix( mdl$beta[1:ncol(Z),] )
  beta <- Matrix( mdl$beta[ ( ncol(Z) + (1:ncol(X)) ), ] )

  # optimality violation
  err <- checkKKT4MSCCS(Z,X,y,w,lambda,alpha,beta)

  return ( list( alpha=alpha, beta=beta, err=err, lambda=lambda) )
}
