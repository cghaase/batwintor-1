#' Calculate energy needed during an arousal from torpor
#'
#' \code{CalcEnergyArousal} Calculate the enegry required to maintain
#'  eurthermic temperatures during an arousal from torpor.
#'
#' @param Ta temperature
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#'
#' @return returns enegry required to arouse
#'
#' @example ExampleScripts/CalcEnergyArousal_ex.R
#'
#' @references McKechnie & Wolf 2004 equation 3
#' @author Katie Haase

CalcEnergyArousal <- function(Ta, bat.params){
	with(bat.params,{
		ifelse(Ta > Ttormin,
					(mass*(Teu - Ta)*S) + ((Teu - Ta)/WR)*(TMRmin + ((RMR - TMRmin)/2)),
					(mass*(Teu - Ttormin)*S) + ((Teu - Ttormin)/WR)*
					  (TMRmin + ((RMR - TMRmin)/2)))
		})
}

