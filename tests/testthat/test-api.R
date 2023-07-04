
# TODO: find way to deal with testing functions which need a secret

test_that("get_return_url() returns correctly encoded url", {
  api_info <- list(base_url = Sys.getenv("PULSE_BASE_URL"))
  return_url <- get_return_url(
    api_info,
    patient_id = Sys.getenv("PATIENT_ID"),
    organisation_id = Sys.getenv("ORGANIZATION_ID"),
    environment = Sys.getenv("APP_ENV")
  )
  expect_false(grepl("http", return_url))
  expect_false(grepl(Sys.getenv("PATIENT_ID"), return_url))
  expect_false(grepl(Sys.getenv("ORGANIZATION_ID"), return_url))
  expect_true(length(return_url) > 0)
})
