#' Fit baseline regularization
#'
#' Fit Baseline Regularization with the given parameters
#' @param parameters parameters object produced by defineBRParameters
#' @param brData data object produced by prepareBRData
#' @return An object representing the fit model, consisting of the main effect weights and baseline biases
#' @export
#' @import Matrix
fitBaselineRegularization <- function( brData, parameters = defineBRParameters() ){

  # Perhaps temporary catch?
  if ( parameters$lambda1 != 0 ) stop( paste( "Nonzero lambda1 not yet supported (was", lambda1, ")." ) )

  # Extract the following from brData:
  X = brData$X # Exposure matrix
  Z = brData$Z # Interval-to-baseline-parameter design matrix.
  l = brData$l # Interval durations
  n = brData$n # # of adverse events in each interval
  segIndx = brData$patients # For each segment indicates which patient it matches

  # Determine the number of baseline parameters
  n_t <- ncol(Z)
  # Determine the number of beta parameters (number of drugs)
  n_beta <- ncol(X)

  # initialize loop
  t <- Matrix(-abs(rnorm(n_t)/10)) # Initializes a column vector to -abs( Normal(0,0.1) )
  beta <- Matrix(rep(0,n_beta)) # Column of zeros

  # lam1 <- parameters$lambda1 # Will presumably implement later
  lam2 <- parameters$lambda2
  lam3 <- parameters$lambda3

  bestBeta <- beta
  bestT <- t
  betaOldOld <- beta
  tOldOld <- t

  absOuter <- 200 # Dummy value that will be changed in iteration?

  # epsilon = 1e-3; # Not used??
  thre <- parameters$threshold
  bestRes = Inf

  for( outerCounter in 1:parameters$maxOuterLoopIterations ){

    # eta
    eta = X %*% beta + Z %*% t;
    selector = eta+log(l)<log(1e-5);
    logAlpha = 0; alpha = 1;

    logW = as.numeric(eta)+log(l); # Why "as numeric??"
    logW[as.logical(selector)] = log(1e-5); # Could just replace with pmin( eta + log( l ), log( epsilon (=1e-5) ) )?

    # working response
    psi = eta - alpha;
    psi[n>0] = psi[n>0]+exp(logAlpha-logW)[n>0]*n[n>0];

    # weight for bwflsa
    brBetaWeights = exp(logW-logAlpha);
    omega = diag(t(Z) %*% Diagonal(x=brBetaWeights) %*% Z)+ 2*lam3;


    for( innerCounter in 1:parameters$maxInnerLoopIterations ){

      # beta step
      brBetaResponse = psi-Z%*%t;
      mdlBeta = glmnet::glmnet(x=X, y=brBetaResponse, family="gaussian",
                       weights = brBetaWeights, alpha=0, lambda=0,
                       intercept=FALSE, thresh = 1e-8, standardize=FALSE);
      betaOld = beta;
      beta = mdlBeta$beta;
      rm(mdlBeta);

      # t step
      tau =  as.numeric(t(Z)%*%(brBetaWeights*(psi-X%*%beta)))/omega;
      tOld = t; t = bflsa::bwflsa(segIndx,tau,omega,lam2); t = Matrix(t);

      # stopping criteria
      absInner = getAbsErr(rBind(tOld,betaOld),rBind(t,beta));

      if(absOuter>10 & absInner<0.05*absOuter){
        break;
      }else if(absInner<max(1e-3*absOuter,thre)){
        break;
      }

    }

    absOuter = getAbsErr(rBind(tOldOld,betaOldOld),rBind(t,beta));

    betaErr = as.numeric( t(X)%*%( n - exp( log(l) + (X%*%beta+Z%*%t) ) ) );
    betaRes = sqrt( mean(betaErr^2) );

    # update best
    if ( betaRes < bestRes ) {
      bestBeta = beta;
      bestT = t;
      bestRes = betaRes;
    }

    # print(paste(
    #   "Indx: ", x,
    #   " HOI: ", dxIdWanted, "; Itr: ", outerCounter,
    #   "; tDf: ", round(length(unique(as.numeric(t)))/nPatient,3),
    #   "; tAbsMax: ", round(max(abs(t))*sign(t[which.max(abs(as.numeric(t)))]),3),
    #   "; absOuter: ", round(absOuter,5),
    #   #             "; alpha:", round(alpha,3),
    #   #             "; epsilon:", round(epsilon,8),
    #   "; err:", round(betaRes,5),
    #   "; inCount:", innerCounter,
    #   "; nBad:", sum(selector),
    #   sep=""));

    if ( absOuter < thre ) {
      break;
    } else {
      tOldOld = t; betaOldOld = beta;
    }

  }

  list( beta = beta, t = t, res=betaRes, parameters = parameters )
}