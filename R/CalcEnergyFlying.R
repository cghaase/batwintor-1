#' Calculate the energy expended during flight
#'
#' \code{CalcEnergyFlying} calculates energry expended during a unit of light
#' @param Ta ambient tempterature
#' @param bat.params see \code{\link{BatLoad}}
#'
#' @return energy expended during a time unit of flight

#' @family Arousal Functions
#'
#' @seealso  \code{\link{CalcEnergyArousal}}, \code{\link{CalcCoolTime}},
#' \code{\link{CalcEnergyCool}}, \code{\link{CalcTimeEuthermic}},
#' \code{\link{CalcEnergyArousal}}, \code{\link{CalcEnergyTimeEuthermic}},
#' \code{\link{CalcEnergyFlying}}, \code{\link{CalcTimeFlying}}
#' @export
CalcEnergyFlying <- function(Ta, bat.params){
  with(bat.params,{
    (RMR * 16.5) + (Tlc-Ta)*Ceu
  })}
