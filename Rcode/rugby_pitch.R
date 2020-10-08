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

  annotate_pitch <- function(colour   = "dimgray",
                             fill     = "white",
                             limits   = TRUE,
                             dimensions = pitch_worldrugby) {
    
    # NOTE: could parameterise the whole function by the list of layer-creation
    #       functions it uses. We could then open up the API for user-defined pitch
    #       elements (e.g. a custom goal type)
    marking_layers <- unlist(list(
      annotate_base_pitch(colour, fill, dimensions),
      # annotate_goal_line(colour, fill, dimensions),
      # annotate_six_yard_box(colour, fill, dimensions),
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
      # Fiels with in-goal
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
        geom = "point",
        x = midpoint$x,
        y = midpoint$y,
        colour = colour,
        fill = fill
      ),
      # Halfway line
      ggplot2::annotate(
        "segment",
        x = midpoint$x,
        xend = midpoint$x,
        y = spec$origin_y,
        yend = spec$origin_y + spec$width,
        colour = colour
      ),
      # Goal line 1
      ggplot2::annotate(
        "segment",
        x = spec$goal_line,
        xend = spec$goal_line,
        y = spec$origin_y,
        yend = spec$origin_y + spec$width,
        colour = colour
      ),
      # Goal line 2
      ggplot2::annotate(
        "segment",
        x = spec$length - spec$goal_line,
        xend = spec$length - spec$goal_line,
        y = spec$origin_y,
        yend = spec$origin_y + spec$width,
        colour = colour
      ),
      # 22-meters line 1
      ggplot2::annotate(
        "segment",
        x = spec$line_22m,
        xend = spec$line_22m,
        y = spec$origin_y,
        yend = spec$origin_y + spec$width,
        colour = colour
      ),
      # 22-meters line 2
      ggplot2::annotate(
        "segment",
        x = spec$length - spec$line_22m,
        xend = spec$length - spec$line_22m,
        y = spec$origin_y,
        yend = spec$origin_y + spec$width,
        colour = colour
      )
    )
  }
  
  
  annotate_goal_line <- function(colour, fill, spec) {
    midpoint <- pitch_center(spec)
    
    list(
      ggplot2::annotate(
        geom = "line",
        xmin = spec$origin_x + spec$length - spec$penalty_box_length,
        xmax = spec$origin_x + spec$length,
        ymin = midpoint$y - spec$goal_line/2,
        ymax = midpoint$y + spec$goal_line/2,
        colour = colour,
        fill = fill
      )
    )
  }
  
  annotate_six_yard_box <- function(colour, fill, spec) {
    midpoint <- pitch_center(spec)
    
    list(
      ggplot2::annotate(
        geom = "rect",
        xmin = spec$origin_x + spec$length - spec$six_yard_box_length,
        xmax = spec$origin_x + spec$length,
        ymin = midpoint$y - spec$six_yard_box_width/2,
        ymax = midpoint$y + spec$six_yard_box_width/2,
        colour = colour,
        fill = fill
      ),
      ggplot2::annotate(
        geom = "rect",
        xmin = spec$origin_x,
        xmax = spec$origin_x + spec$six_yard_box_length,
        ymin = midpoint$y - spec$six_yard_box_width/2,
        ymax = midpoint$y + spec$six_yard_box_width/2,
        colour = colour,
        fill = fill
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
  
  




  
  
  