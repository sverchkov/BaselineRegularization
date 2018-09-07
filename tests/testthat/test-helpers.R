context("Misc helper functions")

test_that("Single strings detected", expect_true( isSingleString( "string" ) ) )
test_that("Multiple strings aren't a single string", expect_false( isSingleString( c( "hello", "world" ) ) ) )
test_that("NULL isn't a single string", expect_false( isSingleString( NULL ) ) )
test_that("Non-strings aren't a single string", expect_false( isSingleString( 1:4 ) ) )

# vectors for testing
x <- c( -1.5852333, -1.5498037,  0.6503323, -0.7126385 )
y <- c( -1.0420216,  1.5332334,  0.5127456,  1.4058937 )
z <- c( -1.5852333, -1.5498037,  0.6503323, -0.7126385, 1.445806 )

test_that("Absolute error between different vectors computed correctly", expect_equal( getAbsErr( x, y ), sqrt( sum( (x-y)^2 ) ) ) )
test_that("Absolute error between vectors and itself is zero", expect_equal( getAbsErr( x, x ), 0 ) )
test_that("Absolute error function throws an error on different sized vectors", expect_error( getAbsErr( x, z ) ) )
