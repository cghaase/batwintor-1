#' Time conversion functions
#'
#' \code{hour.to.month}
#' @param x input value of hours
#' @details general functions for consistant exchange between time units
#' @export
hour.to.month <- function(x){
  x/(24*30)
}
#'
#' \code{hour.to.day}
#' @param x input number of hours
#' @details general functions for consistant exchange between time units
#' @export
hour.to.day <- function(x){
  x/24
}

#' \code{day.to.month}
#' @param x input number of days
#' @details general functions for consistant exchange between time units
#' @export
day.to.month <- function(x){
  x/30
}
#'
#' \code{prop.to.months}
#' @param x precent proportion of the year
#' @details general functions for consistant exchange between time units
#' @export
prop.to.months <- function(x){
  x*(365/30)
}

