#' Calculate energy per time euthermic with ambient temperature
#'
#' \code{CalcEnergyTimeEuthermic} Function calculating the energy required to
#' maintain euthermia for a given time and ambient temperature.
#'
#' @param Ta ambient temperature
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#'
#' @return TODO find units accociated with it
#'
#' @example ExampleScripts/CalEnergyTimeEuthermic_ex.R
CalcEnergyTimeEuthermic <- function(Ta, bat.params){
  with(bat.params,{
    RMR+ (Tlc-Ta)*Ceu
  })}
