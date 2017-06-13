#' Calculate energy needed during an arousal from torpor
#'
#' \code{CalcEnergyArousal} Calculate the enegry required to maintain
#'  eurthermic temperatures during an arousal from torpor.
#'
#' @param Ttor temperature of torpor
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#'
#' @return returns enegry required to arouse
#'
#' @example ExampleScripts/CalcEnergyArousal_ex.R
#'
CalcEnergyArousal <- function(Ttor, bat.params){
  with(bat.params,{
    (Teu - Ttor) * S
  })
}
