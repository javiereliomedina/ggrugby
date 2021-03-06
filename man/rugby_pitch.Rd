% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rugby_pitch.R
\name{rugby_pitch}
\alias{rugby_pitch}
\title{Adds rugby pitch markings as a layer for use in a ggplot plot.}
\usage{
rugby_pitch(
  colour = "white",
  fill = "#7fc47f",
  limits = TRUE,
  dimensions = pitch_worldrugby
)
}
\arguments{
\item{colour}{Colour of pitch outline.}

\item{fill}{Colour of pitch fill}

\item{limits}{Whether to adjust the plot limits to display the whole pitch.}

\item{dimensions}{A list containing the pitch dimensions to draw.}
}
\value{
list of ggplot geoms to be added to a ggplot plot
}
\description{
Adds rugby pitch markings as a layer for use in a ggplot plot.
}
\examples{
library(tidyverse)
library(ggrugby)

team_1 <- tribble(~ID, ~x, ~y,
                  1,  49, 45,
                  2,  49, 44,
                  3,  49, 43,
                  4,  48, 44.5,
                  5,  48, 43.5,
                  6,  48, 42.5,
                  7,  48, 45.5,
                  8,  47, 44,
                  9,  48, 47,
                  10, 39, 35,
                  11, 20, 5,
                  12, 35, 25,
                  13, 30, 15,
                  14, 35, 60,
                  15, 25, 50)

team_2 <- tribble(~ID, ~x, ~y,
                  1,  50, 45,
                  2,  50, 44,
                  3,  50, 43,
                  4,  51, 44.5,
                  5,  51, 43.5,
                  6,  51, 42.5,
                  7,  51, 45.5,
                  8,  52, 44,
                  9,  51, 47,
                  10, 62, 37,
                  11, 80, 12,
                  12, 63, 28,
                  13, 65, 20,
                  14, 62, 57,
                  15, 80, 45)
ggplot() +
  rugby_pitch() +
  theme_minimal() +
  labs(title = "Rugby pitch") +
  geom_point(data = team_1, aes(x = x, y = y), col = "red") +
  geom_point(data = team_2, aes(x = x, y = y), col = "blue")

}
