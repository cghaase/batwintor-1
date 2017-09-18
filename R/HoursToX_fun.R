#' Time conversion functions
#'
#' \code{hour.to.month}
#' \code{hour.to.day}
#' \code{day.to.month}
#' \code{prop.to.month}
#'
#' @param x input value of hours or days
#'
#' @details general functions for consistant exchange between time units
#'
hour.to.month <- function(x){
  x/(24*30)
}

hour.to.day <- function(x){
  x/24
}

day.to.month <- function(x){
  x/30
}

prop.to.months <- function(x){
  x*(365/30)
}

