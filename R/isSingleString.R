#' Check if an object is a single string
#'
#' @param x the object to check.
#' @return TRUE iff x is of type character and of length 1.
#' @author Yuriy Sverchkov
isSingleString <- function( x ) is.character( x ) && length( x ) == 1
