test_that("Testing mapping function gives the right object", {
  # load data
  file_path <- system.file("data", "dt.tsv", package = "PCR")
  dt <- read_tsv(file_path) %>%
    eq_clean_date %>%
    eq_clean_location

  # make plot
  p <- dt %>%
    filter(COUNTRY == 'MEXICO' & year(date) >= 2000) %>%
    mutate(popup_text = eq_create_label(.)) %>%
    eq_map()

  # object class
  expect_is(p, class = c("leaflet", "htmlwidget"))
})
