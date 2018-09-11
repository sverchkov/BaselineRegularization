#' learn a baseline regularization (BR) model
#'
#' learn a BR model from matrix and vector input
#' currently only support ridge penalty onver drug effects
#' currently only support one pair of lambda1 and lambda2
#' learning at a time
#'
#' @param Z design matrix that map tied baseline to
#' baseline on each interval
#'
#' @param interval_obs_period a vector indicating to which observation period/patient each interval belongs.
#'
#' @param X data matirx, each row represent data from
#' one interval, each column represent drug exposure info
#'
#' @param l length of each interval
#'
#' @param n number of condition occurrences on each interval
#'
#' @param lambda1 strength of ridge regression for beta
#'
#' @param labmda2 strengh of fused lasso for baseline
#'
#' @param lambda3 strength of ridge regression for baseline
#'
#' @param ... currently unused
#'
#' @return a br model
#'
#' @export
#' @import futile.logger
#' @author Zhaobin Kuang
fitBR = function(Z,interval_obs_period,X,l,n,lambda1,lambda2,lambda3=0,...){

  ## NOTES:
  ## D - rows are intervals
  ## D$patientId column - our segIdx column
  ## D$baselineIndx - indicates how intervals are tied (captured by Z as well)
  # helper data structure
  # baseline = unique(D[,c("patientId","baselineIndx")])

  # Interval grouping by baseline parameters, using which.max to get around "argument is not logical"
  interval_baseline <- apply( Z, 1, which.max )

  # Mapping of patients to baseline parameters
  baseline_obs_period <- interval_obs_period[ apply( Z, 2, which.max ) ]

  # total number of differences between adjacent basline parameters, which is
  # the number of distinct (tied) baseline parameters minus the number of patients
  n_baseline_parameters <- ncol( Z )
  n_baseline_diff <- n_baseline_parameters - length( unique( interval_obs_period ) )

  # initialize t and beta
  # Use MSCCS to initialize baseline parameters
  model_0 <- fitMSCCS(interval_obs_period,X,l,n,lambda=lambda1,threshold=1e-7)

  t <- model_0$alpha[baseline_obs_period]

  beta = numeric( ncol(X) )

  flog.trace("Starting BR optimization loop...")

  # outer loop
  repeat{

    errOuter <- checkKKT4BR(Z,baseline_obs_period,X,l,n,t,beta,lambda1,lambda2,lambda3)
    flog.trace( "Outer Loop Error: %20.8f", errOuter )
    if ( 1e-6 > errOuter ) break

    log_s = as.numeric(Z%*%t+X%*%beta)
    w = exp(log(l)+log_s)
    z = as.numeric(log_s + n/(w) - 1)
    tTilde = t
    betaTilde = beta
    workingResponse = z-Z%*%tTilde

    if ( errOuter == Inf ){
      flog.trace( "beta:", beta, capture = T )
      flog.trace( "Z*t:", Z%*%t, capture = T )
      flog.trace( "X*beta:", X%*%beta, capture = T )
      flog.trace( "log_s:", log_s, capture = T )
      flog.trace( "w:", w, capture = T )
      flog.trace( "workingResponse:", workingResponse, capture = T )
    }

    # inner loop
    repeat{

      # beta step
      #tryCatch(
      betaTilde <- getWls(y=workingResponse,X=X,w=w,lambda=lambda1*sum(l)/sum(w),thresh=1e-20)
      #, function(e) error( str(e) ) )

      # t step
      omega <- w%*%Z + lambda3

      nu <- ( t( w * ( z - X %*% betaTilde ) ) %*% Z ) / omega

      tTilde = blockwiseWeightedFusedLassoSignalApproximator(indx = baseline_obs_period, y=nu, w=omega, lambda = lambda2 * n_baseline_diff )

      # check inner loop optimality
      workingResponse = z-Z%*%tTilde
      errInner = checkKKT4BetaStep(y=workingResponse,X=X,w=w,beta=betaTilde,l=l,lambda=lambda1)

      flog.trace( "Inner Loop Error: %16.8f", errInner )

      flog.trace( "Beta~:", betaTilde, capture = T )

      if(errInner<1e-6){
        beta = betaTilde
        t = tTilde
        break
      }
    }
  }
  flog.trace( "BR converged.")

  return(list(t=t,beta=beta,err=errOuter))
}






