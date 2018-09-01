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
