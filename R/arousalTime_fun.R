#' Calculate time to arouse from torpor to euthermic temperatures
#'
#' \code{arousalTime} Calculates time to arouse to euthermic conditions
#' given ambient conditions
#'
#' @param Ta ambient temperature
#' @param bat.params list of parameters output from \code{\link{batLoad}}
#'
#' @example ExampleScripts/arousalTime_ex.R
#'
#' @family Arousal Functions
#' @seealso \code{\link{CalcEnergyArousal}}, \code{\link{CalcCoolTime}},
#' \code{\link{coolEnergy}}, \code{\link{euthermicEnergy}},
#' \code{\link{flyingEnergy}}, \code{\link{flyingTime}}
#'
#' @references McKechnie & Wolf 2004 equation 3
#' @export
arousalTime <- function(Ta, bat.params){
  with(bat.params,{
    ifelse(Ta > Ttormin,
           ((Teu - Ta)/WR),
           ((Teu - Ttormin)/WR))
  })
}

