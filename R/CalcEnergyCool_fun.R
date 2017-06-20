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
#' \code{\link{CalcEnergyCool}}, \code{\link{CalcEnergyTimeEuthermic}}
#'
#' @references McKechnie & Wolf 2004 equation 3
#'
#' @author Katie Haase

CalcEnergyCool<- function(Ta, bat.params){
  with(bat.params,{
    ifelse(Ta > Ttormin,
           (((Teu - Ta)/CR)*(TMRmin + ((RMR - TMRmin)/2))),
            (((Teu - Ttormin)/CR)*(TMRmin + ((RMR - TMRmin)/2))))
  })
}
