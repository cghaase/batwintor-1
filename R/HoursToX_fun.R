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
#' @export
hour.to.month <- function(x){
  x/(24*30)
}

#' @export
hour.to.day <- function(x){
  x/24
}

#' @export
day.to.month <- function(x){
  x/30
}

#' @export
prop.to.months <- function(x){
  x*(365/30)
}

