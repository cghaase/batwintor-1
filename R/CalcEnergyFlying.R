#' Calculate the energy expended during flight
#'
#' \code{CalcEnergyFlying} calculates energry expended during a unit of light
#' @param Ta ambient tempterature
#' @param bat.params see \code{\link{BatLoad}}
#'
#' @return energy expended during a time unit of flight
#' @export
CalcEnergyFlying <- function(Ta, bat.params){
  with(bat.params,{
    (RMR * 16.5) + (Tlc-Ta)*Ceu
  })}
