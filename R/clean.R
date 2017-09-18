#' Clean earthquake dates
#'
#' @param df A data.frame such as this obtainded from NOAA.
#'
#' @return A data.frame with a tidy date column.
#' @import dplyr
#' @examples
#' \dontrun{
#' file_path <- system.file("data", "dt.tsv", package = "PCR")
#' dt <- read_tsv(file_path) %>%
#'   eq_clean_date %>%
#' }
#' @export
eq_clean_date <- function(df) {
  df %>%
    dplyr::mutate(date = paste(YEAR, MONTH, DAY, ''),
           date = lubridate::ymd(date))
}

#' Clean earthquake locations
#'
#' @param df A data.frame such as this obtainded from NOAA.
#'
#' @return A data.fram with a tidy location column
#' @examples
#' \dontrun{
#' file_path <- system.file("data", "dt.tsv", package = "PCR")
#' dt <- read_tsv(file_path) %>%
#'   eq_clean_date %>%
#'   eq_clean_location
#' }
#' @import dplyr stringr
#' @export
eq_clean_location <- function(df) {
  df %>%
    dplyr::mutate(LOCATION = stringr::str_split(LOCATION_NAME, ': ', simplify = TRUE)[,2],
           LOCATION = stringr::str_to_title(LOCATION_NAME))
}
