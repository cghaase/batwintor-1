#' Calculate time to cool from euthermic to torpor.
#'
#' \code{coolTime} Calculates time to cool from euthermic conditions to
#' torpid state given the ambient temperature.
#'
#' @param Ta ambient temperature
#'
#' @param bat.params list of parameters output from \code{\link{batLoad}}
#'
#' @return Time in hours.
#'
#' @example ExampleScripts/coolTime_ex.R
#'
#' @references McKechnie & Wolf 2004 equation 3
#'
#' @family Arousal Functions
#'
#'  @seealso \code{\link{arousalTime}}, \code{\link{coolEnergy}},
#' \code{\link{CalcEnergyArousal}}, \code{\link{euthermicEnergy}}
#' \code{\link{flyingEnergy}}, \code{\link{flyingTime}}
#'
#' @author Katie Haase
#' @export
coolTime <- function(Ta, bat.params){
  with(bat.params,{
    ifelse(Ta > Ttormin,
          ((Teu-Ta)/((Ceu*((10*(Mass^0.67)) + SA.wing)*(Teu-Ta))/(0.1728*Mass))),
           ((Teu-Ttormin)/((Ceu*((10*(Mass^0.67)) + SA.wing)*(Teu-Ta))/(0.1728*Mass)))
    )
  }
  )
}
