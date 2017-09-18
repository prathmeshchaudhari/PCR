#' Create earthquake label
#'
#' @param df A data.frame such as this obtainded from NOAA.
#'
#' @return A column of earthquak annotation; locaion, magnitude and deaths. Ignores the fiels when NA.
#' @examples
#' \dontrun{
#' # load and clean data
#' file_path <- system.file("data", "dt.tsv", package = "PCR")
#' dt <- read_tsv(file_path) %>%
#'   eq_clean_date %>%
#'   eq_clean_location
#'
#' # make graph
#' dt %>%
#' filter(COUNTRY == 'MEXICO' & year(date) >= 2000) %>%
#'   mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map()
#' }
#' @import dplyr
#' @export
eq_create_label <- function(df) {
  df %>%
  dplyr::mutate(location = ifelse(is.na(LOCATION), '', paste("<b>Location: </b>", LOCATION, "<br>")),
                magnitude = ifelse(is.na(EQ_PRIMARY), '', paste("<b>Magnitude: </b>", EQ_PRIMARY, "<br>")),
                deaths = ifelse(is.na(DEATHS), '', paste("<b>Total deaths: </b>", DEATHS))) %>%
    dplyr::rowwise() %>%
    do(popup_text = paste(.$location, .$magnitude, .$death)) %>%
    unlist(recursive = FALSE) %>%
    as.character
}

#' Map earthquake data to a leaflet tile
#'
#' @param df A data.frame such as this obtainded from NOAA.
#'
#' @return A graph with markers for specified earthquaks and popups with info; location, magnitude and deaths.
#' @examples
#' \dontrun{
#' # load and clean data
#' file_path <- system.file("data", "dt.tsv", package = "PCR")
#' dt <- read_tsv(file_path) %>%
#'   eq_clean_date %>%
#'   eq_clean_location
#'
#' # make graph
#' dt %>%
#' filter(COUNTRY == 'MEXICO' & year(date) >= 2000) %>%
#'   mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map()
#' }
#' @import leaflet
#' @export
eq_map <- function(df) {
  leaflet::leaflet(df) %>%
    leaflet::addProviderTiles(providers$CartoDB) %>%
    leaflet::addCircleMarkers(~LONGITUDE, ~LATITUDE,
                     popup = ~popup_text,
                     labelOptions = labelOptions(direction = 'bottom'),
                     radius = 5)
}
