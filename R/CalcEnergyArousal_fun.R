#' Calculate energy needed during an arousal from torpor to euthermia.
#'
#' \code{CalcEnergyArousal} Calculate the enegry required to maintain
#'  eurthermic temperatures during an arousal from torpor.
#'
#' @param Ta ambient temperature
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#'
#' @return returns enegry required to arouse from torpor to euthermia
#'
#' @example ExampleScripts/CalcEnergyArousal_ex.R
#'
#' @family Arousal Functions
#'
#' @seealso  \code{\link{CalcEnergyCool}}, \code{\link{CalcCoolTime}},
#' \code{\link{CalcEnergyCool}}, \code{\link{CalcEnergyTimeEuthermic}},
#' \code{\link{CalcEnergyFlying}}, \code{\link{CalcTimeFlying}}
#'
#' @references McKechnie & Wolf 2004 equation 3
#'
#' @author Katie Haase
#' @export

CalcEnergyArousal <- function(Ta, bat.params){
	with(bat.params,{
		ifelse(Ta > Ttormin,
					(mass*(Teu - Ta)*S) + ((Teu - Ta)/WR)*(TMRmin + ((RMR - TMRmin)/2)),
					(mass*(Teu - Ttormin)*S) + ((Teu - Ttormin)/WR)*
					  (TMRmin + ((RMR - TMRmin)/2)))
		})
}

