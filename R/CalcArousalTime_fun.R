#' Calculate the time to arouse from a bout of torpor
#'
#' \code{CalcArousalTime} Calculates the time taken to arouse from a bout of
#' torpor given ambient temperatures.
#'
#' @param Ta ambient temperature
#' @param bat.params parameters returned by \code{BatLoad}
#'
#' @details Extraced from McKechnie & Wolf 2004 equation 3
#' @author Katie Haase
CalcArousalTime <- function(Ta, bat.params){
  with(bat.params,{
    ifelse(Ta > Ttormin,
           ((Teu - Ta)/WR),
           ((Teu - Ttormin)/WR))
  })
}
