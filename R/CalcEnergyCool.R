#' Calculate the enegry needed to cool from eithermic body temperature.
#'
#' \code{CalcEnegryCool} Calculates the energy needed to cool the body from
#' euthermic and return to torpor given abient temperature.
#'
#' @param Ta ambient temperature
#' @param bat.params parameters returned by \code{bat.params}
#'
#' @details Extraced from McKechnie & Wolf 2004 equation 3
#' @author Katie Haase
CalcEnegryCool <- function(Ta, bat.params){
  with(bat.params,{
    ifelse(Ta > Ttormin,
           ((Teu - Ta)/CR)*(TMRmin + ((RMR - TMRmin)/2))
           ((Teu - Ttormin)/CR)*(TMRmin + ((RMR - TMRmin)/2)))
  })
}
