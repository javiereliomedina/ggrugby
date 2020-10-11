#' Adds soccer pitch markings as a layer for use in a ggplot plot.
#'
#' @param colour Colour of pitch outline.
#' @param fill Colour of pitch fill
#' @param limits Whether to adjust the plot limits to display the whole pitch.
#' @param dimensions A list containing the pitch dimensions to draw. See `help(pitch_opta)`.
#'
#' @return list of ggplot geoms to be added to a ggplot plot
#'
#' @examples
#' library(ggplot2)
#'
#' shots_data <- data.frame(x = c(90, 85, 82, 78, 83),
#'                          y = c(43, 40, 52, 56, 44))
#'
#' ggplot(shots_data, aes(x = x, y = y)) +
#'   annotate_pitch() +
#'   geom_point()
#'
#' @export

  annotate_pitch <- function(colour   = "white",
                             fill     = "#7fc47f",
                             limits   = TRUE,
                             dimensions = pitch_worldrugby) {
    
    # NOTE: could parameterise the whole function by the list of layer-creation
    #       functions it uses. We could then open up the API for user-defined pitch
    #       elements (e.g. a custom goal type)
    marking_layers <- unlist(list(
      annotate_base_pitch(colour, fill, dimensions),
      annotate_touchlines(colour, fill, dimensions),
      annotate_inside_lines(colour, fill, dimensions),
      annotate_goal(colour, fill, dimensions)
    ), recursive = FALSE)
    
    if (!limits) {
      return(marking_layers)
    }
    
    # Leave room for full pitch + goals and direction_label by default
    limit_layers <- list(
      ggplot2::xlim(dimensions$origin_x - 3,
                    dimensions$origin_x + dimensions$length + 3),
      ggplot2::ylim(dimensions$origin_y - 5,
                    dimensions$origin_y + dimensions$width + 3)
    )
    
    append(
      marking_layers,
      limit_layers,
    )
  }
  
  # Pitch components -------------------------------------------------------------
  # Add markings for parts of a soccer pitch.
  # NOTE: Should these be exposed for top-level use?
  
  annotate_base_pitch <- function(colour, fill, spec) {
    midpoint <- pitch_center(spec)
    
    list(
      # Field with in-goal
      ggplot2::annotate(
        geom = "rect",
        xmin = spec$origin_x,
        xmax = spec$origin_x + spec$length,
        ymin = spec$origin_y,
        ymax = spec$origin_y + spec$width,
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
        colour = colour,
        fill = fill
      ),
      # Halfway line
      ggplot2::annotate(
        geom = "segment",
        x = midpoint$x,
        xend = midpoint$x,
        y = spec$origin_y,
        yend = spec$origin_y + spec$width,
        colour = colour
      ),
      # Goal line 1
      ggplot2::annotate(
        geom = "segment",
        x = spec$goal_line,
        xend = spec$goal_line,
        y = spec$origin_y,
        yend = spec$origin_y + spec$width,
        colour = colour
      ),
      # Goal line 2
      ggplot2::annotate(
        geom = "segment",
        x = spec$length - spec$goal_line,
        xend = spec$length - spec$goal_line,
        y = spec$origin_y,
        yend = spec$origin_y + spec$width,
        colour = colour
      ),
      # 22-meters line 1
      ggplot2::annotate(
        geom = "segment",
        x = spec$goal_line + 22,
        xend = spec$goal_line + 22,
        y = spec$origin_y,
        yend = spec$origin_y + spec$width,
        colour = colour
      ),
      # 22-meters line 2
      ggplot2::annotate(
        geom = "segment",
        x = spec$length - spec$goal_line - 22,
        xend = spec$length - spec$goal_line - 22,
        y = spec$origin_y,
        yend = spec$origin_y + spec$width,
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
        x = rep(spec$goal_line + 5, 6),
        xend = rep(spec$goal_line + 5, 6),
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
        x = rep(spec$length - spec$goal_line - 5, 6),
        xend = rep(spec$length - spec$goal_line - 5, 6),
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
        x = c(spec$goal_line + 5,
              spec$goal_line + 21,
              midpoint$x - 9,
              midpoint$x - 1,
              midpoint$x + 9,
              spec$length - spec$goal_line - 21,
              spec$length - spec$goal_line - 5),
        xend = c(spec$goal_line + 7,
                 spec$goal_line + 23,
                 midpoint$x - 11,
                 midpoint$x + 1,
                 midpoint$x + 11,
                 spec$length - spec$goal_line - 23,
                 spec$length - spec$goal_line - 7),
        
        y = rep(spec$origin_y + 5, 7),
        yend = rep(spec$origin_y + 5, 7),
        colour = colour
      ),
      # 15-meters line 1
      ggplot2::annotate(
        geom = "segment",
        x = c(spec$goal_line + 5,
              spec$goal_line + 21,
              midpoint$x - 9,
              midpoint$x - 1,
              midpoint$x + 9,
              spec$length - spec$goal_line - 21,
              spec$length - spec$goal_line - 5),
        xend = c(spec$goal_line + 7,
                 spec$goal_line + 23,
                 midpoint$x - 11,
                 midpoint$x + 1,
                 midpoint$x + 11,
                 spec$length - spec$goal_line - 23,
                 spec$length - spec$goal_line - 7),
        
        y = rep(spec$origin_y + 15, 7),
        yend = rep(spec$origin_y + 15, 7),
        colour = colour
      ),
      # 5-meters line 2
      ggplot2::annotate(
        geom = "segment",
        x = c(spec$goal_line + 5,
              spec$goal_line + 21,
              midpoint$x - 9,
              midpoint$x - 1,
              midpoint$x + 9,
              spec$length - spec$goal_line - 21,
              spec$length - spec$goal_line - 5),
        xend = c(spec$goal_line + 7,
                 spec$goal_line + 23,
                 midpoint$x - 11,
                 midpoint$x + 1,
                 midpoint$x + 11,
                 spec$length - spec$goal_line - 23,
                 spec$length - spec$goal_line - 7),
        y = rep(spec$origin_y + spec$width - 5, 7),
        yend = rep(spec$origin_y + spec$width - 5, 7),
        colour = colour
      ),
      # 15-meters line 2
      ggplot2::annotate(
        geom = "segment",
        x = c(spec$goal_line + 5,
              spec$goal_line + 21,
              midpoint$x - 9,
              midpoint$x - 1,
              midpoint$x + 9,
              spec$length - spec$goal_line - 21,
              spec$length - spec$goal_line - 5),
        xend = c(spec$goal_line + 7,
                 spec$goal_line + 23,
                 midpoint$x - 11,
                 midpoint$x + 1,
                 midpoint$x + 11,
                 spec$length - spec$goal_line - 23,
                 spec$length - spec$goal_line - 7),
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
        x = spec$length - spec$goal_line,
        xend = spec$length - spec$goal_line + goal_depth,
        y = midpoint$y - spec$goal_width/2,
        yend = midpoint$y + spec$goal_width/2,
        colour = colour,
        fill = fill
      ),
      ggplot2::annotate(
        geom = "segment", 
        x = spec$length - spec$goal_line,
        xend = spec$length - spec$goal_line + goal_depth,
        y = midpoint$y - spec$goal_width/2 + spec$goal_width,
        yend = midpoint$y + spec$goal_width/2 + spec$goal_width,
        colour = colour,
        fill = fill
      ),
      ggplot2::annotate(
        geom = "segment", 
        x = spec$length - spec$goal_line + goal_depth/2,
        xend = spec$length - spec$goal_line + goal_depth/2,
        y = midpoint$y - spec$goal_width/2 + spec$goal_width/2,
        yend = midpoint$y + spec$goal_width/2 + spec$goal_width/2,
        colour = colour,
        fill = fill
      ),
      
      ggplot2::annotate(
        geom = "segment", 
        x = spec$goal_line - goal_depth,
        xend = spec$goal_line,
        yend = midpoint$y - spec$goal_width/2,
        y = midpoint$y + spec$goal_width/2,
        colour = colour,
        fill = fill
      ),
      ggplot2::annotate(
        geom = "segment", 
        x = spec$goal_line - goal_depth,
        xend = spec$goal_line,
        yend = midpoint$y - spec$goal_width/2 + spec$goal_width,
        y = midpoint$y + spec$goal_width/2 + spec$goal_width,
        colour = colour,
        fill = fill
      ),
      ggplot2::annotate(
        geom = "segment", 
        xend = spec$goal_line - goal_depth/2,
        x = spec$goal_line - goal_depth/2,
        yend = midpoint$y - spec$goal_width/2 + spec$goal_width/2,
        y = midpoint$y + spec$goal_width/2 + spec$goal_width/2,
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
  
  




  
  
  