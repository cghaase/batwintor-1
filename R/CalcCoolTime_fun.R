#' Calculate time to cool from euthermic to torpor.
#'
#' \code{CalcCoolTime} Calculates time to cool from euthermic conditions to
#' torpid state given the ambient temperature.
#'
#' @param Ta ambient temperature
#' @param bat.params list of parameters output from \code{\link{BatLoad}}
#' @return Time in hours.
#' @details
#' @examples ExampleScripts/CalcCoolTime_ex.R
#'
#' @references McKechnie & Wolf 2004 equation 3
#' @family Arousal Functions
#' @seealso \code{\link{CalcArousalTime}}, \code{\link{CalcEnergyCool}},
#' \code{\link{CalcEnergyArousal}}, \code{\link{CalcEnergyTimeEuthermic}}
#' @author Katie Haase
#'
CalcCoolTime <- function(Ta, bat.params){
  with(bat.params,{
    ifelse(Ta > Ttormin,
           ((Teu - Ta)/CR),
           ((Teu - Ttormin)/CR))
  }
  )
}
