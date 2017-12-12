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
#' @family Arousal Functions
#'
#' @seealso  \code{\link{CalcEnergyCool}}, \code{\link{CalcCoolTime}},
#' \code{\link{CalcEnergyCool}}, \code{\link{CalcEnergyCool}},
#' \code{\link{CalcEnergyFlying}}, \code{\link{CalcTimeFlying}}
#'
#' @example ExampleScripts/CalcEnergyTimeEuthermic_ex.R
#' @export
CalcEnergyTimeEuthermic <- function(Ta, bat.params){
  with(bat.params,{
    RMR+ (Tlc-Ta)*Ceu
  })}
