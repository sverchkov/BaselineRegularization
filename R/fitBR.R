#' Fit baseline regularization
#'
#' Fit Baseline Regularization
#'
#' @param interval_baseline_parameter An array where each position corresponds to an interval and each value
#' corresponds to the index of a baseline parameter
#' @param baseline_parameter_obs_period An array where each position corresponds to a baseline parameter and each
#' value corresponds to the index of an observation period
#' @export
#' @import Matrix
#' @import futile.logger
#' @author Zhaobin Kuang
fitBR <- function( interval_baseline_parameter, baseline_parameter_obs_period, X, l, n, lambda1, lambda2, lambda3, ... ){

  dots <- list( ... )
  max_outer_loop_iterations <- setIfNull( dots$max_outer_loop_iterations, 1000 )
  max_inner_loop_iterations <- setIfNull( dots$max_inner_loop_iterations, 1000 )

  ## Note:
  # What is mathematically Z %*% v for vector v can be replaced by v[interval_baseline_parameter]
  # Similarly, t(Z) %*% v becomes sumBy( v, interval_baseline_parameter )

  # Determine the number of baseline parameters
  n_t <- length( baseline_parameter_obs_period )

  # Determine the number of beta parameters (number of drugs)
  n_beta <- ncol(X)

  # initialize loop with msccs
  msccs_model <- fitMSCCS( baseline_parameter_obs_period[ interval_baseline_parameter ], X, l, n, lambda1 )

  t <- msccs_model$alpha[ baseline_parameter_obs_period ]
  beta <- Matrix(rep(0,n_beta)) # Column of zeros

  bestBeta <- beta
  bestT <- t
  betaOldOld <- beta
  tOldOld <- t

  absOuter <- 200 # Dummy value that will be changed in iteration?

  # epsilon = 1e-3; # Not used??
  thre <- 1e-6
  bestRes = Inf

  for( outerCounter in 1:max_outer_loop_iterations ){

    flog.trace( "Outer loop iteration %s", outerCounter )

    # eta
    eta <- X %*% beta + t[ interval_baseline_parameter ];
    selector <- eta + log(l) < log(1e-5);
    logAlpha <- 0; alpha = 1;

    logW <- as.numeric(eta) + log(l); # Why "as numeric??"
    logW[ as.logical(selector) ] <- log(1e-5); # Could just replace with pmin( eta + log( l ), log( epsilon (=1e-5) ) )?

    # working response
    psi = eta - alpha;
    psi[n>0] <- psi[n>0]+exp(logAlpha-logW)[n>0]*n[n>0];

    # weight for bwflsa
    brBetaWeights <- exp( logW - logAlpha )
    omega = sumBy( x = brBetaWeights, groups = interval_baseline_parameter ) + 2*lam3;


    for( innerCounter in 1:max_inner_loop_iterations ){

      flog.trace( "Inner loop iteration %s", innerCounter )

      # beta step
      brBetaResponse = psi - t[ interval_baseline_parameter ];

      betaOld <- beta

      if ( 0 == lam1 ) {
        beta_fit <- stats::lsfit( x=X, y=brBetaResponse, wt=brBetaWeights, intercept = FALSE, tolerance = 1e-8 )
        beta <- Matrix( beta_fit$coefficients )
      } else {
        # Note: glmnet doesn't like only getting one lambda, so we give it a sequence and then grab the one we need.
        mdlBeta <- glmnet::glmnet( x=X, y=brBetaResponse, family="gaussian",
                                   weights = brBetaWeights, alpha=0, lambda=lam1*c(100,10,1),
                                   intercept=FALSE, thresh = 1e-8, standardize=FALSE);
        beta <- Matrix( mdlBeta$beta[,3] );
        rm(mdlBeta);
      }

      # t step
      tau <- sumBy( brBetaWeights * ( psi - X%*%beta ), interval_baseline_parameter )/omega
      tOld <- t
      t <- blockwiseWeightedFusedLassoSignalApproximator( baseline_parameter_obs_period, tau, omega, lam2 )
      t <- Matrix(t)

      # stopping criteria
      absInner <- getAbsErr( rbind( tOld, betaOld ), rbind( t, beta ) );

      if ( absOuter > 10 & absInner < 0.05 * absOuter ){
        break;
      }else if ( absInner < max( 1e-3 * absOuter, thre ) ){
        break;
      }

    }

    absOuter <- getAbsErr( rbind( tOldOld, betaOldOld ), rbind( t, beta ) );

    betaErr <- as.numeric( t(X) %*% ( n - exp( log(l) + ( X%*%beta + t[interval_baseline_parameter] ) ) ) )
    betaRes <- sqrt( mean(betaErr^2) );

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

  list( beta = beta, t = t, res=betaRes, msccs = msccs_model )
}
