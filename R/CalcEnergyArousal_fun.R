#' Calculate energy needed during an arousal from torpor
#'
#' \code{CalcEnegryArousal} Calculate the energy required to arouse from torpor
#' and enter an euthermic state.
#' @param Ta ambient temperature
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#'
#' @return returns enegry required to arouse from torpor
#'
#' @example ExampleScripts/CalcEnergyArousal_ex.R
#'

#' @details Extraced from McKechnie & Wolf 2004 equation 3
#' @example ExampleScripts/CalcEnegryArousal_ex.R
#' @author Katie Haase
CalcEnergyArousal <- function(Ttor, bat.params){
  with(bat.params,{
    ifelse(Ta > Ttormin,
           (mass*(Teu - Ta)*S) + ((Teu - Ta)/WR)*(TMRmin + ((RMR - TMRmin)/2)))
           (mass*(Teu - Ttomin)*S) + ((Teu - Ttormin)/WR)*(TMRmin + ((RMR -
            TMRmin)/2))
  })
}
