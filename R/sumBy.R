sumBy = function( x, groups ){
  result <- sapply( 1:max(groups), function ( g ) sum( x[ groups == g ] ) )
}
