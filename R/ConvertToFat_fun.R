#' Convert kCal to grams of fat
#'
#' \code{ConvertToFat} converts kCal to grams of fat.
#'
#' @param energy amount of enegry to convert in kCal
#' @param k1 constant 1
#' @param k2 constant 2
#' @param k3 constant 3
#' @example ExampleScripts/ConvertToFat_ex.R
#' @export
ConvertToFat <- function(energy, k1 = 20.1, k2 = 39.3, k3 = 1000){
  energy*k1/(k2*k3)
}
