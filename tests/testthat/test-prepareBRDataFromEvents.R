context("prepare data from event table")

library(Matrix)

events_data <- read.csv(
  system.file("testdata", "events_example.csv", package="BaselineRegularization", mustWork = T),
  stringsAsFactors = FALSE,
  colClasses = c("numeric", "numeric", "Date", "numeric", "Date", "Date", "numeric") )

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
# TODO: check that this is accurate
expectedL <- c( 638, 1, 439, 56, 2035, 659, 60, 364, 1, 276, 302, 887, 390, 701 )
expectedN <- c(   0, 0,   1,  0,    0,   0,  0,   1, 1,   1,   0,   0,   1,   0 )
expectedP <- c(   1, 1,   1,  1,    1,   2,  2,   2, 2,   2,   2,   2,   2,   2 )
expectedOccZ <- Matrix( data = c( 1, 0, 0, 0, 0, 0, 0 # PT 1
                                , 1, 0, 0, 0, 0, 0, 0
                                , 0, 1, 0, 0, 0, 0, 0 # PT 1, Event 1
                                , 0, 1, 0, 0, 0, 0, 0
                                , 0, 1, 0, 0, 0, 0, 0
                                , 0, 0, 1, 0, 0, 0, 0 # PT 2
                                , 0, 0, 1, 0, 0, 0, 0
                                , 0, 0, 0, 1, 0, 0, 0 # PT 2, Event 1
                                , 0, 0, 0, 0, 1, 0, 0 # PT 2, Event 2
                                , 0, 0, 0, 0, 0, 1, 0 # PT 2, Event 3
                                , 0, 0, 0, 0, 0, 1, 0
                                , 0, 0, 0, 0, 0, 1, 0
                                , 0, 0, 0, 0, 0, 0, 1 # PT 2, Event 4
                                , 0, 0, 0, 0, 0, 0, 1 )
                      , ncol = 7, byrow = T )

test_that("interval tying on example",{
  br_data <- prepareBRDataFromEvents( events_data, event = 1, tying = "interval" )

  # X is as specified in expectedX
  expect_equal( dim( br_data$X ), c( 14, 2 ) )
  expect_true( all( br_data$X == expectedX ) )

  expect_true( isDiagonal( br_data$Z ) ) # Z is diagonal for interval tying
  expect_equal( dim( br_data$Z ), c( 14, 14 ) ) # Z is 14x14 for interval tying

  expect_true( all( br_data$l == expectedL ) )

  expect_true( all( br_data$n == expectedN ) )

  expect_true( all( br_data$patients == expectedP ) )
})

test_that("occurrence tying on example",{
  br_data <- prepareBRDataFromEvents( events_data, event = 1, tying = "occurrence" )

  # X is as specified in expectedX
  expect_equal( dim( br_data$X ), c( 14, 2 ) )
  expect_true( all( br_data$X == expectedX ) )

  expect_true( all( br_data$Z == expectedOccZ ) ) # Z is as expected
  expect_equal( dim( br_data$Z ), c( 14, 7 ) ) # Z is 14x7 for occurrence tying

  expect_true( all( br_data$l == expectedL ) )

  expect_true( all( br_data$n == expectedN ) )

  expect_true( all( br_data$patients == expectedP ) )
})
