#' Calculate time to cool from euthermic temperatures
#'
#' \code{CalcCoolTime} Calculates time to cool from euthermic conditions
#'
#' @param Ta ambient temperature
#' @param bat.params list of parameters output from \code{\link{BatLoad}}
#'
#' @details
#' @examples ExampleScripts/CalcCoolTime_ex.R
#'
#' @references McKechnie & Wolf 2004 equation 3
#'
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
