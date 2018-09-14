context("fitBR")

test_that("Regularized mini interval-tying single-patient model fit", {

  # Tiny toy dataset:
  # * 1 patients, with 14 intervals
  # * 2 drugs,
  # Interval length: 5 2 1 5 10 10 5 10 4 7 3 12 20 12
  # ADE:             0 0 1 0  1  0 0  0 0 1 0  0  0  0
  # Drug1:           0 1 1 1  1  0 0  0 1 1 1  1  0  0
  # Drug2:           1 1 1 1  1  1 0  0 0 0 0  0  0  0

  X <- Matrix::Matrix( c(
    0, 1,
    1, 1,
    1, 1,
    1, 1,
    1, 1,
    0, 1,
    0, 0,
    0, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    0, 0,
    0, 0
    ), nrow=14, ncol=2 )
  Z <- diag( 14 )
  l <- c( 5, 2, 1, 5, 10, 10, 5, 10, 4, 7, 3, 12, 20, 12 )
  n <- c( 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0 )
  interval_obs_period = rep( 1, 14 )

  fit = fitBR( Z, interval_obs_period, X, l, n, lambda1 = 0.01, lambda2=0.05, lambda3 = 0 )

})
