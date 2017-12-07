#' conversion functions
#'
#' \code{hour.to.month}
#' @param x input value of hours
#' @details general functions for consistant exchange between time units
#' @export
hour.to.month <- function(x){
  x/(24*30)
}
#' \code{month.to.hour}
#' @param x input value of months
#' @details general functions for consistant exchange between time units
#' @export
month.to.hour <- function(x){
  x*24*30
}
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

#' \code{day.to.hour}
#' @param x days to convert
#' @details general functions for consistant exchange between time units
#' @export
day.to.hour <- function(x){
  x*24
}

#' \code{mW_ml02hr}
#' @param mW energy in milliWatts
#' @return energy in ml02/hr
#' @export
mW_mlO2hr <- function(mW){
  Watts = mW*0.001
  Jhr = Watts * 60
  mO2hr = Jhr*20.1
  return(mO2hr)
}

#' \code{k.to.c}
#' @param x temperature in degrees Kelvin
#' @return temperature in degrees Celcius
#' @export
k.to.c <- function(x){
  x + 273
}

