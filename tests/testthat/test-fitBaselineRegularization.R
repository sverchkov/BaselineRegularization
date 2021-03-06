context("fit baseline regularization")

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
  interval_baseline_parameter = 1:14,
  baseline_parameter_obs_period = rep( c(1:4), c(5,3,4,2) ),
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

test_that("nonzero lambda1 raises error", {
  expect_error( fitBaselineRegularization( NULL, defineBRParameters( lambda1 = 0.5 ) ) )
})

test_that("mini interval-tying model fit", {

  fit = fitBaselineRegularization( brData )

  expect_equal( as.numeric( fit$beta ), c(-2.063499229, -1.882488127, -20.426202260) )
})

test_that("regularized mini interval-tying model fit", {

  fit = fitBaselineRegularization( brData, defineBRParameters( lambda1 = 0.01 ) )

  expect_equal( as.numeric( fit$beta ), c( 0.02679898, 0.16094389, -0.56575141) )
})
