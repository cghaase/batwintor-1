#' Calculate energy needed during a cooling to torpor
#'
#' \code{coolEnergy} Calculate the energy required to cool to torpid temperatures
#'
#' @param Ta ambient temperature
#' @param bat.params list of bat parameters output from \code{\link{batLoad}}
#'
#' @return returns energy required to cool
#'
#' @example ExampleScripts/coolEnergy_ex.R
#'
#' @family Arousal Functions
#'
#' @seealso  \code{\link{CalcEnergyArousal}}, \code{\link{CalcCoolTime}},
#' \code{\link{coolEnergy}}, \code{\link{euthermicEnergy}},
#' \code{\link{flyingEnergy}}, \code{\link{flyingTime}}
#'
#' @references McKechnie & Wolf 2004 equation 3
#'
#' @author Katie Haase
#' @export

coolEnergy<- function(Ta, bat.params){
  with(bat.params,{
    ifelse(Ta > Ttormin,
           ((((Ceu + Ct)/2)*(Teu-Ta)*mass) + (TMRmin + ((RMR - TMRmin)/2)))*((Teu - Ta)/CR),
           ((((Ceu + Ct)/2)*(Teu-Ttormin)*mass) + (TMRmin + ((RMR - TMRmin)/2)))*((Teu - Ttormin)/CR))
  })
}
