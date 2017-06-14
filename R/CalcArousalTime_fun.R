#' Calculate time to arouse to euthermic temperatures
#'
#' \code{CalcArousalTime} Calculates time to arouse to euthermic conditions
#'
#' @param Ta ambient temperature
#' @param bat.params list of parameters output from \code{\link{BatLoad}}
#'
#' @details
#' @examples ExampleScripts/CalcArousalTime_ex.R
#'
#' @references McKechnie & Wolf 2004 equation 3

CalcArousalTime <- function(Ta, bat.params){
  with(bat.params,{
    ifelse(Ta > Ttormin,
           ((Teu - Ta)/WR),
           ((Teu - Ttormin)/WR))
  })
}

