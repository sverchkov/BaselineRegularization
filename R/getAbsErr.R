#' Get absolute error between a pair of vectors/matrices
#'
#' Get absolute error between a pair of vectors/matrices.
#' Produces error messages if the objects have different dimensions.
#'
#' @param a A vector/matrix
#' @param b A vector/matrix
#' @return Frobenius norm between `a` and `b`.
#'
#' @import Matrix
#' @import futile.logger
#' @author Zhaobin Kuang
#' @author Yuriy Sverchkov
getAbsErr = function( a, b ){
  oldVec = Matrix( a );
  newVec = Matrix( b );

  d1 <- dim( oldVec )
  d2 <- dim( newVec )
  if( any( d1 != d2 ) ){
    flog.error( "Trying to compare vectors of different size! (%s and %s)",
                deparse( enexpr( a ) ),
                deparse( enexpr( b ) ) )
    flog.error( "Dimensions of first argument:", d1, capture = T )
    flog.error( "Dimensions of second argument:", d2, capture = T )
  }

  relErr = norm(oldVec-newVec,type="F");
  return(relErr);
}
