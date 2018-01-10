#' Calculate energy needed during an arousal from torpor to euthermia.
#'
#' \code{arousalEnergy} Calculate the enegry required to maintain
#'  eurthermic temperatures during an arousal from torpor.
#'
#' @param Ta ambient temperature
#' @param bat.params list of bat parameters output from \code{\link{batLoad}}
#'
#' @return returns enegry required to arouse from torpor to euthermia
#'
#' @example ExampleScripts/arousalEnergy_ex.R
#'
#' @family Arousal Functions
#'
#' @seealso  \code{\link{coolEnergy}}, \code{\link{coolTime}},
#'  \code{\link{euthermicEnergry}},
#' \code{\link{flyingEnergy}}, \code{\link{flyingTime}}
#'
#' @references McKechnie & Wolf 2004 equation 3
#'
#' @author Katie Haase
#' @export

arousalEnergy <- function(Ta, bat.params){
	with(bat.params,{
		ifelse(Ta > Ttormin,
					(mass*(Teu - Ta)*S) + ((Teu - Ta)/WR)*(TMRmin + ((RMR - TMRmin)/2)),
					(mass*(Teu - Ttormin)*S) + ((Teu - Ttormin)/WR)*
					  (TMRmin + ((RMR - TMRmin)/2)))
		})
}

