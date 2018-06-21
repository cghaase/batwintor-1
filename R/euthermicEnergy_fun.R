#' Calculate energy per time euthermic with ambient temperature
#'
#' \code{euthermicEnergy} Function calculating the energy required to
#' maintain euthermia for a given time and ambient temperature.
#'
#' @param Ta ambient temperature
#' @param bat.params list of bat parameters output from \code{\link{batLoad}}
#'
#' @return TODO find units accociated with it
#'
#' @family Arousal Functions
#'
#' @seealso  \code{\link{coolEnergy}}, \code{\link{coolTime}},
#' \code{\link{flyingEnergy}}, \code{\link{flyingTime}}
#'
#' @example ExampleScripts/euthermicEnergy_ex.R
#' @export
euthermicEnergy <- function(Ta, bat.params){
  with(bat.params,{
    RMR+ (Tlc-Ta)*Ceu
  })}
