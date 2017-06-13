#' Calculate energy needed during a cooling to torpor
#'
#' \code{CalcEnergyCool} Calculate the energy required to cool to torpid temperatures
#'
#' @param Ta temperature 
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#'
#' @return returns energy required to cool
#'
#'

calcCoolTime <- function(Ta, bat.params){
  with(as.list(params),{
    ifelse(Ta > Ttormin, 
           ((Teu - Ta)/CR),                  #McKechnie & Wolf 2004 equation 3
           ((Teu - Ttormin)/CR))
  }
  )			
}
