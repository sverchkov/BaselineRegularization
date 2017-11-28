context("fit baseline regularization")

test_that("nonzero lambda1 raises error", {
  expect_error( fitBaselineRegularization( NULL, defineBRParameters( lambda1 = 0.5 ) ) )
})

test_that("mini interval-tying model fit", {

  # Tiny toy dataset:
  # * 4 patients, with 5, 3, 4, 2 intervals respectively
  # * 3 drugs, a, b, c. x marks adverse event, - marks empty interval, (#) marks length
  # Patient 1: c(5) -(2) x(1) a(5) ax(10)
  # Patient 2: a(10) ab(5) b(10)
  # Patient 3: a(4) abx(7) b(3) bc(12)
  # Patient 4: abc(20) -(12)

  brData <- list(
    X = Matrix::t( Matrix::Matrix(
      c( 0,0,1, 0,0,0, 0,0,0, 1,0,0, 1,0,0,
         1,0,0, 1,1,0, 0,1,0,
         1,0,0, 1,1,0, 0,1,0, 0,1,1,
         1,1,1, 0,0,0 ),
      nrow=3, ncol=14 ) ),
    Z = diag( 14 ),
    l = c( 5, 2, 1, 5, 10,
           10, 5, 10,
           4, 7, 3, 12,
           20, 12 ),
    n = c( 0, 0, 1, 0, 1,
           0, 0, 0,
           0, 1, 0, 0,
           0, 0 ),
    patients = c( rep( 1, 5 ), rep( 2, 3 ), rep( 3, 4 ), rep( 5, 2 ) )
  )

  fit = fitBaselineRegularization( brData )

  expect_equal( fit$res, 4.664561e-05 )
})
