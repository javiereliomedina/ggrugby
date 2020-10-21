#' Adds rugby pitch markings as a layer for use in a ggplot plot.
#'
#' @param colour Colour of pitch outline.
#' @param fill Colour of pitch fill
#' @param limits Whether to adjust the plot limits to display the whole pitch.
#' @param dimensions A list containing the pitch dimensions to draw.
#'
#' @return list of ggplot geoms to be added to a ggplot plot
#'
#' @examples
#' library(tidyverse)
#' library(ggrugby)
#'
#' team_1 <- tribble(~ID, ~x, ~y,
#'                   1,  49, 45,
#'                   2,  49, 44,
#'                   3,  49, 43,
#'                   4,  48, 44.5,
#'                   5,  48, 43.5,
#'                   6,  48, 42.5,
#'                   7,  48, 45.5,
#'                   8,  47, 44,
#'                   9,  48, 47,
#'                   10, 39, 35,
#'                   11, 20, 5,
#'                   12, 35, 25,
#'                   13, 30, 15,
#'                   14, 35, 60,
#'                   15, 25, 50)
#'
#' team_2 <- tribble(~ID, ~x, ~y,
#'                   1,  50, 45,
#'                   2,  50, 44,
#'                   3,  50, 43,
#'                   4,  51, 44.5,
#'                   5,  51, 43.5,
#'                   6,  51, 42.5,
#'                   7,  51, 45.5,
#'                   8,  52, 44,
#'                   9,  51, 47,
#'                   10, 62, 37,
#'                   11, 80, 12,
#'                   12, 63, 28,
#'                   13, 65, 20,
#'                   14, 62, 57,
#'                   15, 80, 45)
#' ggplot() +
#'   rugby_pitch() +
#'   theme_minimal() +
#'   labs(title = "Rugby pitch") +
#'   geom_point(data = team_1, aes(x = x, y = y), col = "red") +
#'   geom_point(data = team_2, aes(x = x, y = y), col = "blue")
#'
#' @export

rugby_pitch <- function(colour   = "white",
                        fill     = "#7fc47f",
                        limits   = TRUE,
                        dimensions = pitch_worldrugby) {
  marking_layers <- unlist(list(
    annotate_base_pitch(colour, fill, dimensions),
    annotate_touchlines(colour, fill, dimensions),
    annotate_inside_lines(colour, fill, dimensions),
    annotate_goal(colour, fill, dimensions)
  ), recursive = FALSE)
  }

# Pitch components -------------------------------------------------------------
# Add markings for parts of a rugby pitch.

annotate_base_pitch <- function(colour, fill, spec) {
  midpoint <- pitch_center(spec)

  list(
    # Field
    ggplot2::annotate(
      geom = "rect",
      xmin = spec$origin_x - spec$dead_line,
      xmax = spec$origin_x + spec$length + spec$dead_line,
      ymin = spec$origin_y,
      ymax = spec$origin_y + spec$width,
      colour = colour,
      fill = fill
    ),
    # Lines
    ggplot2::annotate(
      geom = "segment",
      x = c(spec$origin_x,
            spec$origin_x + 22,
            midpoint$x,
            spec$length - spec$origin_x - 22,
            spec$length - spec$origin_x),
      xend = c(spec$origin_x,
               spec$origin_x + 22,
               midpoint$x,
               spec$length - spec$origin_x - 22,
               spec$length - spec$origin_x),
      y = rep(spec$origin_y, 5),
      yend = rep(spec$origin_y + spec$width, 5),
      colour = colour,
      fill = fill
    ),
    # Centre spot
    ggplot2::annotate(
      geom = "segment",
      x = midpoint$x - 0.5,
      xend = midpoint$x + 0.5,
      y = midpoint$y,
      yend = midpoint$y,
      colour = colour
    )
  )
}

annotate_inside_lines <- function(colour, fill, spec) {
  midpoint <- pitch_center(spec)

  list(
    # 5-meters line 1
    ggplot2::annotate(
      geom = "segment",
      x = rep(spec$origin_x + 5, 6),
      xend = rep(spec$origin_x + 5, 6),
      y = c(spec$origin_y + 4,
            spec$origin_y + 14,
            midpoint$y - 4,
            midpoint$y + 4,
            spec$width - 4,
            spec$width - 14),
      yend = c(spec$origin_y + 6,
               spec$origin_y + 16,
               midpoint$y - 6,
               midpoint$y + 6,
               spec$width - 6,
               spec$width - 16),
      colour = colour
    ),
    # 5-meters line 2
    ggplot2::annotate(
      geom = "segment",
      x = rep(spec$length - spec$origin_x - 5, 6),
      xend = rep(spec$length - spec$origin_x - 5, 6),
      y = c(spec$origin_y + 4,
            spec$origin_y + 14,
            midpoint$y - 4,
            midpoint$y + 4,
            spec$width - 4,
            spec$width - 14),
      yend = c(spec$origin_y + 6,
               spec$origin_y + 16,
               midpoint$y - 6,
               midpoint$y + 6,
               spec$width - 6,
               spec$width - 16),
      colour = colour
    ),
    # 10-meters line 1
    ggplot2::annotate(
      geom = "segment",
      x = rep(midpoint$x - 10, 6),
      xend = rep(midpoint$x - 10, 6),
      y = c(spec$origin_y + 4,
            spec$origin_y + 14,
            midpoint$y - 4,
            midpoint$y + 4,
            spec$width - 4,
            spec$width - 14),
      yend = c(spec$origin_y + 6,
               spec$origin_y + 16,
               midpoint$y - 6,
               midpoint$y + 6,
               spec$width - 6,
               spec$width - 16),
      colour = colour
    ),
    # 10-meters line 2
    ggplot2::annotate(
      geom = "segment",
      x = rep(midpoint$x + 10, 6),
      xend = rep(midpoint$x + 10, 6),
      y = c(spec$origin_y + 4,
            spec$origin_y + 14,
            midpoint$y - 4,
            midpoint$y + 4,
            spec$width - 4,
            spec$width - 14),
      yend = c(spec$origin_y + 6,
               spec$origin_y + 16,
               midpoint$y - 6,
               midpoint$y + 6,
               spec$width - 6,
               spec$width - 16),
      colour = colour
    )

  )
}

annotate_touchlines <- function(colour, fill, spec) {
  midpoint <- pitch_center(spec)

  list(
    # 5-meters line 1
    ggplot2::annotate(
      geom = "segment",
      x = c(spec$origin_x + 5,
            spec$origin_x + 21,
            midpoint$x - 9,
            midpoint$x - 1,
            midpoint$x + 9,
            spec$length - spec$origin_x - 21,
            spec$length - spec$origin_x - 5),
      xend = c(spec$origin_x + 7,
               spec$origin_x + 23,
               midpoint$x - 11,
               midpoint$x + 1,
               midpoint$x + 11,
               spec$length - spec$origin_x - 23,
               spec$length - spec$origin_x - 7),
      y = rep(spec$origin_y + 5, 7),
      yend = rep(spec$origin_y + 5, 7),
      colour = colour
    ),
    # 15-meters line 1
    ggplot2::annotate(
      geom = "segment",
      x = c(spec$origin_x + 5,
            spec$origin_x + 21,
            midpoint$x - 9,
            midpoint$x - 1,
            midpoint$x + 9,
            spec$length - spec$origin_x - 21,
            spec$length - spec$origin_x - 5),
      xend = c(spec$origin_x + 7,
               spec$origin_x + 23,
               midpoint$x - 11,
               midpoint$x + 1,
               midpoint$x + 11,
               spec$length - spec$origin_x - 23,
               spec$length - spec$origin_x - 7),

      y = rep(spec$origin_y + 15, 7),
      yend = rep(spec$origin_y + 15, 7),
      colour = colour
    ),
    # 5-meters line 2
    ggplot2::annotate(
      geom = "segment",
      x = c(spec$origin_x + 5,
            spec$origin_x + 21,
            midpoint$x - 9,
            midpoint$x - 1,
            midpoint$x + 9,
            spec$length - spec$origin_x - 21,
            spec$length - spec$origin_x - 5),
      xend = c(spec$origin_x + 7,
               spec$origin_x + 23,
               midpoint$x - 11,
               midpoint$x + 1,
               midpoint$x + 11,
               spec$length - spec$origin_x - 23,
               spec$length - spec$origin_x - 7),
      y = rep(spec$origin_y + spec$width - 5, 7),
      yend = rep(spec$origin_y + spec$width - 5, 7),
      colour = colour
    ),
    # 15-meters line 2
    ggplot2::annotate(
      geom = "segment",
      x = c(spec$origin_x + 5,
            spec$origin_x + 21,
            midpoint$x - 9,
            midpoint$x - 1,
            midpoint$x + 9,
            spec$length - spec$origin_x - 21,
            spec$length - spec$origin_x - 5),
      xend = c(spec$origin_x + 7,
               spec$origin_x + 23,
               midpoint$x - 11,
               midpoint$x + 1,
               midpoint$x + 11,
               spec$length - spec$origin_x - 23,
               spec$length - spec$origin_x - 7),
      y = rep(spec$origin_y + spec$width - 15, 7),
      yend = rep(spec$origin_y + spec$width - 15, 7),
      colour = colour
    )
  )
}

annotate_goal <- function(colour, fill, spec) {
  midpoint <- pitch_center(spec)
  goal_depth <- 2

  list(
    ggplot2::annotate(
      geom = "segment",
      x = c(spec$length - spec$origin_x,
            spec$length - spec$origin_x,
            spec$length - spec$origin_x + goal_depth/2,
            spec$origin_x - goal_depth,
            spec$origin_x - goal_depth,
            spec$origin_x - goal_depth/2),
      xend = c(spec$length - spec$origin_x + goal_depth,
               spec$length - spec$origin_x + goal_depth,
               spec$length - spec$origin_x + goal_depth/2,
               spec$origin_x,
               spec$origin_x,
               spec$origin_x - goal_depth/2),
      y = c(midpoint$y - spec$goal_width/2,
            midpoint$y - spec$goal_width/2 + spec$goal_width,
            midpoint$y - spec$goal_width/2 + spec$goal_width/2,
            midpoint$y + spec$goal_width/2,
            midpoint$y + spec$goal_width/2 + spec$goal_width,
            midpoint$y + spec$goal_width/2 + spec$goal_width/2),
      yend = c(midpoint$y + spec$goal_width/2,
               midpoint$y + spec$goal_width/2 + spec$goal_width,
               midpoint$y + spec$goal_width/2 + spec$goal_width/2,
               midpoint$y - spec$goal_width/2,
               midpoint$y - spec$goal_width/2 + spec$goal_width,
               midpoint$y - spec$goal_width/2 + spec$goal_width/2),
      colour = colour,
      fill = fill
    )
  )
}

# Helper functions
pitch_center <- function(spec) {
  list(x = spec$origin_x + spec$length/2,
       y = spec$origin_y + spec$width/2)
}








