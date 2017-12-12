#' Calculate the time spent flying
#'
#' \code{calcTimeFlying} calculates the time spent flying as a proportion of
#' time in euthermic arrousals
#' @param bat.params see \code{\link{BatLoad}}
#'
#' @return a time
#' @family Arousal Functions
#'
#' @seealso  \code{\link{CalcEnergyArousal}}, \code{\link{CalcCoolTime}},
#' \code{\link{CalcEnergyCool}}, \code{\link{CalcEnergyTimeEuthermic}},
#' \code{\link{CalcEnergyFlying}}, \code{\link{CalcTimeFlying}}
#' @export
CalcTimeFlying <- function(bat.params){
  with(bat.params,{
    pFly*teu
  })
}
