#' Convert kCal to grams of fat
#'
#' \code{ConvertToFat} converts kCal to grams of fat.
#'
#' @param enegry amount of enegry to convert in kCal
#' @param k1 constant 1
#' @param k2 constant 2
#' @param k3 constant 3
#'
#' @details TODO check and make sure units are correct
#'
#' @example
#' ConvertToFat(enegry = 9)
ConvertToFat <- function(enegry, k1 = 20.1, k2 = 39.3, k3 = 1000){
  enegry*k1/(k2*k3)
}
