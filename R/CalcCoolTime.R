#' Calculate time to cool from euthermic temperatures
#'
#' \code{calcCoolTime} Calculates time to cool from euthermic conditions
#'
#' @param Ta ambient temperature
#' @param bat.params list of parameters output from \code{\link{BatLoad}} 
#'
#' @details TODO
#' @examples TODO
#' 

calcCoolTime <- function(Ta, bat.params){
  with(as.list(params),{
    ifelse(Ta > Ttormin, 
           ((Teu - Ta)/CR),                  #McKechnie & Wolf 2004 equation 3
           ((Teu - Ttormin)/CR))
  }
  )			
}
