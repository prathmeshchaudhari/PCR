% Generated by roxygen2: do not edit by hand
\name{geom_timeline}
\alias{geom_timeline}
\title{Timeline earthquake graph}
\usage{
geom_timeline(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...)
}
\arguments{
\item{mapping}{Set of aesthetics created by aes.}

\item{data}{A data.frame such as that obtained from NOAA}

\item{stat}{A string of statistical transfromation.}

\item{position}{Position adjustment.}

\item{na.rm}{A logical expression for dealing with missing values.}

\item{show.legend}{A logical expression for showing legend or not.}

\item{inherit.aes}{A logical expression of whether or not override default aesthetics.}

\item{...}{Other arguments passed to layer}
}
\description{
Timeline earthquake graph
}
\details{
A timeline plot is a representation of individual earthquakes ordered by their
 corresponding dates as points; the color of the point represent the number of deaths
 that resulted from the event and the size represents the magnitude of the event.
}
\examples{
\dontrun{
# load and clean data
file_path <- system.file("data", "dt.tsv", package = "PCR")
dt <- read_tsv(file_path) \%>\%
  eq_clean_date \%>\%
  eq_clean_location

# make graph
dt \%>\%
filter(COUNTRY == 'MEXICO') \%>\%
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
}
}
