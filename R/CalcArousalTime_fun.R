#' Calculate time to arouse from torpor to euthermic temperatures
#'
#' \code{CalcArousalTime} Calculates time to arouse to euthermic conditions
#' given ambient conditions
#'
#' @param Ta ambient temperature
#' @param bat.params list of parameters output from \code{\link{BatLoad}}
#'
#' @example ExampleScripts/CalcArousalTime_ex.R
#'
#' @family Arousal Functions
#' @seealso \code{\link{CalcEnergyArousal}}, \code{\link{CalcCoolTime}},
#' \code{\link{CalcEnergyCool}}, \code{\link{CalcEnergyTimeEuthermic}},
#' \code{\link{CalcEnergyFlying}}, \code{\link{CalcTimeFlying}}
#'
#' @references McKechnie & Wolf 2004 equation 3
#' @export
CalcArousalTime <- function(Ta, bat.params){
  with(bat.params,{
    ifelse(Ta > Ttormin,
           ((Teu - Ta)/WR),
           ((Teu - Ttormin)/WR))
  })
}

