test_that("Testing Objects of Visualising Functions", {
  # load data
  file_path <- system.file("data", "dt.tsv", package = "PCR")
  dt <- read_tsv(file_path) %>%
    eq_clean_date %>%
    eq_clean_location

  # make plot
  p <- dt %>%
    filter(COUNTRY == 'MEXICO') %>%
    ggplot(aes(date = date,
               xmin = as.Date('1995-01-01'),
               xmax = as.Date('2000-12-30'),
               y = COUNTRY,
               colour = DEATHS,
               fill = DEATHS,
               size = EQ_PRIMARY,
               location = LOCATION)) +
    geom_timeline() +
    geom_timeline_label() +
    theme(axis.line.y = element_blank(),
          axis.line.x = element_line(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'bottom',
          panel.grid = element_blank(),
          panel.background = element_blank())

  # plot object
  expect_is(p, class = c('gg', 'ggplot'))
})


test_that("ploting function passes the right mapping aes", {
  # load data
  file_path <- system.file("data", "dt.tsv", package = "PCR")
  dt <- read_tsv(file_path) %>%
    eq_clean_date %>%
    eq_clean_location

  # make plot
  p <- dt %>%
    filter(COUNTRY == 'MEXICO') %>%
    ggplot(aes(date = date,
               xmin = as.Date('1995-01-01'),
               xmax = as.Date('2000-12-30'),
               y = COUNTRY,
               colour = DEATHS,
               fill = DEATHS,
               size = EQ_PRIMARY,
               location = LOCATION)) +
    geom_timeline() +
    geom_timeline_label() +
    theme(axis.line.y = element_blank(),
          axis.line.x = element_line(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'bottom',
          panel.grid = element_blank(),
          panel.background = element_blank())

  # mapping objects
  expect_identical(p$mapping$y, as.name('COUNTRY'))
  expect_identical(p$mapping$date, as.name('date'))
  expect_identical(p$mapping$colour, as.name('DEATHS'))
  expect_identical(p$mapping$fill, as.name('DEATHS'))
  expect_identical(p$mapping$size, as.name('EQ_PRIMARY'))
  expect_identical(p$mapping$location, as.name('LOCATION'))
})
