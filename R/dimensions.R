#' Pitch dimensions
#'
#' @description The coordinate system used to generate pitch markings 
#' It can be customised by supplying a pitch specification to the `dimensions`
#' argument of `rugby_pitch`.
#'
#' ggrugby provide the pitch specification of World Rugby by default.
#' (https://laws.worldrugby.org/?law=1&language=EN)
#' However, user-defined specifications can also be used.
#'
#' @details A "pitch specification" is simply a list of dimensions that define a
#' coordinate system. The required dimensions are:
#'
#' \itemize{
#'  \item{"length"}{The length of the pitch from one goal line to the other (x axis)}
#'  \item{"width"}{The width of the pitch from one touchline to the other (y axis)}
#'  \item{"dead_line"}{The distance from the dead-ball line to the goal line}
#'  \item{"goal_width"}{The distance from one goal post to the other}
#'  \item{"origin_x"}{The minimum x coordinate of the pitch}
#'  \item{"origin_y"}{The minimum y coordinate of the pitch}
#' }
#'
#' The following pitch dimensions are provided
#' \itemize{
#'  \item{"pitch_worldrugby"}{For World Rugby data}   
#'  }

  pitch_worldrugby <- list(
    length = 100,
    width = 70,
    dead_line = 6,
    goal_width = 5.6,
    origin_x = 0,
    origin_y = 0
  )
  
