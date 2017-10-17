#' Calculate maximum time in torpor
#'
#' \code{CalcTorporTime} Calculate time of a torpor bout given ambient
#' temperature and infection status
#'
#' @param Ta ambient temperature
#' @param areaPd area of pd
#' @param inf infection status (TRUE or FALSE)
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#' @param q  TODO
#'
#' @return returns time bat spends in torpor
#'
#' @example ExampleScripts/CalcTorporTime_ex.R
#' @export
CalcTorporTime <- function(Ta, areaPd, inf, bat.params, q=CalcQ(Ta)){
  with(bat.params,{
    ttor <- ifelse(Ta > Ttormin,
                   (ttormax/q^((Ta - Ttormin)/10)),
                   ttormax/(1+(Ttormin - Ta) * Ct/TMRmin))
    if(inf==TRUE){
      areaPd <- ifelse(areaPd < 1, 1, areaPd)
      ttor <- ttor/areaPd # how fungal growth reduceds time torpid
    }
    return(ttor)
  })
}
