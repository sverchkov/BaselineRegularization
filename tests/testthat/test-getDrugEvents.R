context("Test drug duration to event conversion")

drug_durations <- read.csv(
  system.file("testdata", "durations_example.csv", package="BaselineRegularization", mustWork = T),
  colClasses = "integer" )

drug_events_risk_0 <- read.csv(
  system.file("testdata", "drug_events_risk_0.csv", package="BaselineRegularization", mustWork = T),
  colClasses = "integer" )

drug_events_risk_39 <- read.csv(
  system.file("testdata", "drug_events_risk_39.csv", package="BaselineRegularization", mustWork = T),
  colClasses = "integer" )

drug_events_risk_lasting <- read.csv(
  system.file("testdata", "drug_events_risk_lasting.csv", package="BaselineRegularization", mustWork = T),
  colClasses = "integer" )

test_that("Zero (default) risk window processing",{
  expect_equivalent( getDrugEvents( drug_durations ), drug_events_risk_0 )
})

test_that("39 day risk window processing",{
  result <- getDrugEvents( drug_durations, risk_window = 39 )
  # print( result )
  expect_equivalent( result, drug_events_risk_39 )
})

test_that("Lasting risk is processing",{
  expect_equivalent( getDrugEvents( drug_durations, risk_window = Inf ), drug_events_risk_lasting )
})
