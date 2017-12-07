#' Calculate the time spent flying
#'
#' \code{calcTimeFlying} calculates the time spent flying as a proportion of
#' time in euthermic arrousal
#' @param pFly proportion of euthermia spent flying
#' @param bat.params see \code{\link{BatLoad}}
#'
#' @return a time
#' @family Arousal Functions
#'
#' @seealso  \code{\link{CalcEnergyArousal}}, \code{\link{CalcCoolTime}},
#' \code{\link{CalcEnergyCool}}, \code{\link{CalcEnergyTimeEuthermic}},
#' \code{\link{calcEnergyFlying}}, \code{\link{CalcTimeFlying}}
#' @export
CalcTimeFlying <- function(pFly, bat.params){
  with(bat.params,{
    pFly*teu
  })
}
