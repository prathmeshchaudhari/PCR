
test_that("Testing Output Class of Cleaning functions", {
  # load data
  file_path <- system.file("data", "dt.tsv", package = "PCR")
  dt <- read_tsv(file_path) %>%
    eq_clean_date %>%
    eq_clean_location

  # object classes
  expect_is(signif$date, class = 'Date')
  expect_is(signif$LOCATION, class = 'character')
})
