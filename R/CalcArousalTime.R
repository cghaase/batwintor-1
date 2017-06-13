#' Calculate time to arouse to euthermic temperatures
#'
#' \code{calcArousalTime} Calculates time to arouse to euthermic conditions
#'
#' @param Ta ambient temperature
#' @param bat.params list of parameters output from \code{\link{BatLoad}} 
#'
#' @details TODO
#' @examples TODO
#' 

calcArousalTime <- function(Ta, bat.params){
  with(as.list(params),{
    ifelse(Ta > Ttormin, 
           ((Teu - Ta)/WR),                  #McKechnie & Wolf 2004 equation 3
           ((Teu - Ttormin)/WR))
  }
  )			
}
	
