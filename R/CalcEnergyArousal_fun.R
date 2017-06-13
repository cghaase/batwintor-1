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

calcEnergyArousal <- function(Ta, bat.params){
	with(as.list(params),{
		ifelse(Ta > Ttormin, 
					(mass*(Teu - Ta)*S) + ((Teu - Ta)/WR)*(TMRmin + ((RMR - TMRmin)/2)),                  #McKechnie & Wolf 2004 equation 3
					(mass*(Teu - Ttormin)*S) + ((Teu - Ttormin)/WR)*(TMRmin + ((RMR - TMRmin)/2)))
		}
	)			
}

