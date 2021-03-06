#' Feature matrix builder
#'
#' This builds the feature matrix out of flag vectors
#' (internally called function)
#'
#' @param number_of_intervals The number of intervals
#' @param number_of_features The number of features
#' @param interval_numbers Array of interval numbers (1-based indeces) for flags
#' @param feature_numbers Array of feature numbers (1-based indeces) for flags
#' @param flags Array of +1/-1/0 indicating intervals at which feature durations start/end
#' @return A matrix with `number_of_intervals` rows and `number_of_features` columns, where features are filled in with
#' 1s according to the flags turning them on/off.
buildFeatureMatrix <- function ( number_of_intervals, number_of_features, interval_numbers, feature_numbers, flags ) {

  number_of_intervals <- as.integer( number_of_intervals )
  if ( length( number_of_intervals ) != 1 ){
    err <- "Non-scalar 'number of intervals' passed to buildFeatureMatrix."
    flog.error( err )
    stop( err )
  }

  number_of_features <- as.integer( number_of_features )
  if ( length( number_of_features ) != 1 ){
    err <- "Non-scalar 'number of features' passed to buildFeatureMatrix."
    flog.error( err )
    stop( err )
  }

  interval_numbers <- as.integer( interval_numbers )
  if ( any( interval_numbers > number_of_intervals ) ){
    err <- "Feature matrix interval numbers out of bounds"
    flog.error( err )
    stop( err )
  }

  feature_numbers <- as.integer( feature_numbers )
  if ( any( feature_numbers > number_of_features ) ){
    err <- "Feature matrix interval numbers out of bounds"
    flog.error( err )
    stop( err )
  }

  flags <- as.integer( flags )
  if ( length( interval_numbers ) != length( feature_numbers ) || length( feature_numbers ) != length( flags ) ){
    err <- "Array length mismatch in input to buildFeatureMatrix."
    flog.error( err )
    stop( err )
  }

  flog.trace( "%s intervals, %s features, %s events.", number_of_intervals, number_of_features, length( flags ) )

  X <- sparseMatrix( i = interval_numbers
                     , j = feature_numbers
                     , x = flags
                     , dims = c( number_of_intervals, number_of_features ) )

  # Fill exposure matrix (we marked the start of each exposure with a 1 and the day after the end with a -1 above. By
  # adding the value of the previous cell to each cell we get 1s in every interval during which the patient is exposed )

  ijs <- do.call( cbind, Map( function( feat ){
    i_s <- which( cumsum( X[, feat] ) > 0 )
    j_s <- rep( feat, length(i_s) )
    rbind( i_s, j_s )
  }, 1:number_of_features ) )

  X <- sparseMatrix( i = ijs[1,], j = ijs[2,], dims = c( number_of_intervals, number_of_features ) )

  return( X )
}
