#' Calculates the time taken to cool from euthermia to torpor
#'
#' \code{CalcCoolTime} calculates the time taken to cool the body from
#' euthermia and return to torpor.
#'
#' @param Ta ambient temperature
#' @param bat.params parameters returned by \code{bat.params}
#' @details Extraced from McKechnie & Wolf 2004 equation 3
#' @author Katie Haase
#'
CalcCoolTime <- function(Ta, bat.params){
  with(bat.params,{
    iflese(Ta > Ttormin,
           ((Teu - Ta)/CR),
           ((Teu - Ttormin)/CR))
  })
}
