context("Feature Matrix Builder Helper")

# Load data
load( system.file("testdata", "bfmparams.RData", package="BaselineRegularization", mustWork = T) )

test_that("Raises error on nonscalar number of intervals", expect_error(
  buildFeatureMatrix( c( number_of_intervals, number_of_intervals ),
                      number_of_features,
                      interval_numbers,
                      feature_numbers,
                      flags ),
  "Non-scalar"
) )

test_that("Raises error on nonscalar number of features", expect_error(
  buildFeatureMatrix( number_of_intervals, c( number_of_features, number_of_features ), interval_numbers, feature_numbers, flags ),
  "Non-scalar"
) )

test_that("Raises error on out of bounds intervals", expect_error(
  buildFeatureMatrix( 1L, number_of_features, interval_numbers, feature_numbers, flags ),
  "out of bounds"
) )

test_that("Raises error on out of bounds features", expect_error(
  buildFeatureMatrix( number_of_intervals, 1L, interval_numbers, feature_numbers, flags ),
  "out of bounds"
) )

test_that("Raises error on vector length mismatches", {
  expect_error( buildFeatureMatrix( number_of_intervals, number_of_features, interval_numbers[-1], feature_numbers, flags ), "mismatch" )
  expect_error( buildFeatureMatrix( number_of_intervals, number_of_features, interval_numbers, feature_numbers[-1], flags ), "mismatch" )
  expect_error( buildFeatureMatrix( number_of_intervals, number_of_features, interval_numbers, feature_numbers, flags[-1] ), "mismatch" )
} )

test_that("Builds matrix nominally",{
  expected <- readRDS( system.file( "testdata", "feature-matrix.rds", package="BaselineRegularization", mustWork = T ) )
  expect_equal(
    buildFeatureMatrix( number_of_intervals, number_of_features, interval_numbers, feature_numbers, flags ),
    expected )
})
