context("prepare data from event table")

library(Matrix)

events_data <- read.csv(
  system.file("testdata", "events_example.csv", package="BaselineRegularization", mustWork = T),
  stringsAsFactors = FALSE)

expectedX <- Matrix( data = c( 0, 1 # PT 1
                             , 1, 1
                             , 1, 1
                             , 1, 0
                             , 0, 0
                             , 0, 0 # PT 2
                             , 0, 1
                             , 0, 1
                             , 0, 1
                             , 0, 1
                             , 0, 0
                             , 0, 1
                             , 0, 1
                             , 0, 0 )
                   , ncol = 2, byrow = T )

expectedL <-
  c( 638, 1, 439, 56, 2035, 659, 60, 364, 1, 276, 302, 887, 390, 701 )
expectedN <-
  c(   0, 0,   1,  0,    0,   0,  0,   1, 1,   1,   0,   0,   1,   0 )
expected_int_bp_obs <-
  c(   1, 1,   1,  1,    1,   2,  2,   2, 2,   2,   2,   2,   2,   2 )
expected_occ_interval_bp <-
  c(   1, 1,   2,  2,    2,   3,  3,   4, 5,   6,   6,   6,   7,   7 )
expected_occ_bp_obs <-
  c(   1,      1,             2,       2, 2,   2,             2 )

test_that("interval tying on example",{
  br_data <- prepareBRDataFromEvents( events_data, event = 1, tying = "interval" )

  # X is as specified in expectedX
  expect_equal( dim( br_data$X ), c( 14, 2 ) )
  expect_true( all( br_data$X == expectedX ) )

  expect_equal( br_data$interval_baseline_parameter, 1:14 ) # interval-parameter map is 1:1 for interval tying

  expect_equal( br_data$baseline_parameter_obs_period, expected_int_bp_obs )

  expect_true( all( br_data$l == expectedL ) )

  expect_true( all( br_data$n == expectedN ) )
})

test_that("occurrence tying on example",{
  br_data <- prepareBRDataFromEvents( events_data, event = 1, tying = "occurrence" )

  # X is as specified in expectedX
  expect_equal( dim( br_data$X ), c( 14, 2 ) )
  expect_true( all( br_data$X == expectedX ) )

  expect_equal( br_data$interval_baseline_parameter, expected_occ_interval_bp )

  expect_equal( br_data$baseline_parameter_obs_period, expected_occ_bp_obs )

  expect_true( all( br_data$l == expectedL ) )

  expect_true( all( br_data$n == expectedN ) )
})
