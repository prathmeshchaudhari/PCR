#' Timeline earthquake graph
#'
#' @param mapping Set of aethetics created by aes.
#' @param data A data.fram such as that obtained from NOAA
#' @param stat A string of statistical transfromation.
#' @param position Position adjustment.
#' @param na.rm A logical for dealing with missing values.
#' @param show.legend A logical for showing legend or not.
#' @param inherit.aes A logical of whether or not override default aesthetics.
#' @param ... Other arguments passed to layer
#'
#' @details A timeline plot is a representation of individual earthquaks ordered by their
#'  corresponding dates as points; the color of the point represent the number of deaths
#'  that resulted from the event and the size represents the magnitude of the event.
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
#' filter(COUNTRY == 'MEXICO') %>%
#'   ggplot(aes(date = date,
#'              xmin = as.Date('1995-01-01'),
#'              xmax = as.Date('2000-12-30'),
#'              y = COUNTRY,
#'              colour = DEATHS,
#'              fill = DEATHS,
#'              size = EQ_PRIMARY,
#'              location = LOCATION)) +
#'   geom_timeline() +
#'   geom_timeline_label() +
#'   theme(axis.line.y = element_blank(),
#'         axis.line.x = element_line(),
#'         axis.ticks.y = element_blank(),
#'         axis.text.y = element_blank(),
#'         axis.title.y = element_blank(),
#'         legend.position = 'bottom',
#'         panel.grid = element_blank(),
#'         panel.background = element_blank())
#' }
#' @import ggplot2 dplyr grid
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Geom object for drawing timeline
#'
#' @format NULL
#' @usage NULL
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline",
                                 ggplot2::Geom,
                                 required_aes = c("date", "xmin", "xmax", "y", "colour", "size"),
                                 default_aes = ggplot2::aes(shape = 19, stroke = .5, alpha = .7),
                                 draw_key = function(data, params, size) {
                                   grid::pointsGrob(0.5, 0.5,
                                                    pch = data$shape,
                                                    gp = grid::gpar(fontsize = data$size * .pt + data$stroke * .stroke / 2)
                                   )
                                 },
                                 draw_panel = function(data, panel_scales, coord) {
                                   # prepare data
                                   ## subset by xmin and xmax
                                   data <- data %>%
                                     dplyr::filter(date >= xmin, date <= xmax) %>%
                                     dplyr::mutate(x = as.numeric(date))

                                   coords <- coord$transform(data, panel_scales)
                                   grid::grobTree(
                                     grid::pointsGrob(
                                       coords$x, coords$y,
                                       pch = coords$shape,
                                       gp = grid::gpar(col = alpha(coords$colour, coords$alpha),
                                                       fill = alpha(coords$fill, coords$alpha),
                                                       fontsize = coords$size * .pt + coords$stroke * .stroke / 2)
                                     ),
                                     grid::segmentsGrob(
                                       x0 = unit(coords$xmin, 'npc'),
                                       x1 = unit(coords$xmax, 'npc'),
                                       y0 = unit(coords$y, 'npc'),
                                       y1 = unit(coords$y, 'npc'),
                                       gp = grid::gpar(col = 'gray')
                                     )
                                   )
                                 }
)

#' Timeline label
#'
#' @param mapping Set of aethetics created by aes.
#' @param data A data.fram such as that obtained from NOAA
#' @param stat A string of statistical transfromation.
#' @param position Position adjustment.
#' @param na.rm A logical for dealing with missing values.
#' @param show.legend A logical for showing legend or not.
#' @param inherit.aes A logical of whether or not override default aesthetics.
#' @param ... Other arguments passed to layer.
#'
#' @details Adds labels for the locations of individual earthquaks. Used with geom_timeline.
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
#' filter(COUNTRY == 'MEXICO') %>%
#'   ggplot(aes(date = date,
#'              xmin = as.Date('1995-01-01'),
#'              xmax = as.Date('2000-12-30'),
#'              y = COUNTRY,
#'              colour = DEATHS,
#'              fill = DEATHS,
#'              size = EQ_PRIMARY,
#'              location = LOCATION)) +
#'   geom_timeline() +
#'   geom_timeline_label() +
#'   theme(axis.line.y = element_blank(),
#'         axis.line.x = element_line(),
#'         axis.ticks.y = element_blank(),
#'         axis.text.y = element_blank(),
#'         axis.title.y = element_blank(),
#'         legend.position = 'bottom',
#'         panel.grid = element_blank(),
#'         panel.background = element_blank())
#' }
#' @import ggplot2 dplyr grid
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = FALSE,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' Geom object for drawing timeline labels
#'
#' @format NULL
#' @usage NULL
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimeline",
                                      ggplot2::Geom,
                                      required_aes = c("date", "xmin", "xmax", "location"),
                                      draw_panel = function(data, panel_params, coord) {
                                        # prepare data
                                        ## subset by xmin and xmax
                                        data <- data %>%
                                          dplyr::filter(date >= xmin, date <= xmax) %>%
                                          dplyr::mutate(x = as.numeric(date))
                                        coords <- coord$transform(data, panel_params)

                                        txt <- grid::textGrob(label = coords$location,
                                                              x = unit(coords$x, 'npc'),
                                                              y = unit(coords$y + .2, 'npc'),
                                                              hjust = 0,
                                                              rot = 45,
                                                              gp = grid::gpar(fontsize = 8))
                                        segments <- grid::segmentsGrob(x0 = unit(coords$x, 'npc'),
                                                                       x1 = unit(coords$x, 'npc'),
                                                                       y0 = unit(coords$y, 'npc'),
                                                                       y1 = unit(coords$y + .15, 'npc'),
                                                                       gp = grid::gpar(col = 'gray'))
                                        grid::grobTree(txt, segments)
                                      }
)


#' Modified theme to use with geom_timeline
theme_timeline <- ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                                 axis.line.x = ggplot2::element_line(),
                                 axis.ticks.y = ggplot2::element_blank(),
                                 axis.text.y = ggplot2::element_blank(),
                                 axis.title.y = ggplot2::element_blank(),
                                 legend.position = 'bottom',
                                 panel.grid = ggplot2::element_blank(),
                                 panel.background = ggplot2::element_blank())
