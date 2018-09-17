context("Misc helper functions")

test_that("Single strings detected", expect_true( isSingleString( "string" ) ) )
test_that("Multiple strings aren't a single string", expect_false( isSingleString( c( "hello", "world" ) ) ) )
test_that("NULL isn't a single string", expect_false( isSingleString( NULL ) ) )
test_that("Non-strings aren't a single string", expect_false( isSingleString( 1:4 ) ) )
