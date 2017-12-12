#' Calculate energy needed during a cooling to torpor
#'
#' \code{CalcEnergyCool} Calculate the energy required to cool to torpid temperatures
#'
#' @param Ta ambient temperature
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#'
#' @return returns energy required to cool
#'
#' @example ExampleScripts/CalcEnergyCool_ex.R
#'
#' @family Arousal Functions
#'
#' @seealso  \code{\link{CalcEnergyArousal}}, \code{\link{CalcCoolTime}},
#' \code{\link{CalcEnergyCool}}, \code{\link{CalcEnergyTimeEuthermic}},
#' \code{\link{CalcEnergyFlying}}, \code{\link{CalcTimeFlying}}
#'
#' @references McKechnie & Wolf 2004 equation 3
#'
#' @author Katie Haase
#' @export

CalcEnergyCool<- function(Ta, bat.params){
  with(bat.params,{
    ifelse(Ta > Ttormin,
           ((((Ceu + Ct)/2)*(Teu-Ta)*mass) + (TMRmin + ((RMR - TMRmin)/2)))*((Teu - Ta)/CR),
           ((((Ceu + Ct)/2)*(Teu-Ttormin)*mass) + (TMRmin + ((RMR - TMRmin)/2)))*((Teu - Ttormin)/CR))
  })
}
