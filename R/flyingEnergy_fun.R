#' Calculate the energy expended during flight
#'
#' \code{flyingEnergy} calculates energry expended during a unit of light
#' @param Ta ambient tempterature
#' @param bat.params see \code{\link{batLoad}}
#'
#' @return energy expended during a time unit of flight

#' @family Arousal Functions
#'
#' @seealso  \code{\link{CalcEnergyArousal}}, \code{\link{CalcCoolTime}},
#' \code{\link{coolEnergy}}, \code{\link{euthermicTime}},
#' \code{\link{CalcEnergyArousal}}, \code{\link{euthermicEnergy}},
#' \code{\link{flyingEnergy}}, \code{\link{flyingTime}}
#' @export
flyingEnergy <- function(Ta, bat.params){
  with(bat.params,{
    (RMR * 16.5)
  })}
