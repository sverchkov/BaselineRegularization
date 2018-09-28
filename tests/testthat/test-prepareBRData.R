context("Prepare data")

data("synpuf_mini")
response_event = 137829 # The concept id for "Aplastic anemia"

test_that("Prepare data invoked with defaults", {
  skip("test in development")
  br_data <- prepareBRData(
    observation_period = synpuf_mini$observation_period,
    condition_occurrence = synpuf_mini$condition_occurrence,
    drug_exposure = synpuf_mini$drug_exposure,
    response_event = response_event )
})

test_that("Using connection (sqlite)", {

  skip_if_not_installed( "DBI" )
  skip_if_not_installed( "RSQLite" )

  con <- DBI::dbConnect( RSQLite::SQLite(), dbname = ":memory:" )
  DBI::dbWriteTable( con, "observation_period", synpuf_mini$observation_period )
  DBI::dbWriteTable( con, "condition_occurrence", synpuf_mini$condition_occurrence )
  DBI::dbWriteTable( con, "drug_exposure", synpuf_mini$drug_exposure )

  br_data_db <- prepareBRData( con, response_event = response_event )


  DBI::dbDisconnect( con )

  br_data_tbl <- prepareBRData(
    observation_period = synpuf_mini$observation_period,
    condition_occurrence = synpuf_mini$condition_occurrence,
    drug_exposure = synpuf_mini$drug_exposure,
    response_event = response_event )

  expect_equal( br_data_db, br_data_tbl )
})
