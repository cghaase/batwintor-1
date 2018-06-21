#' Calculate energy needed during a cooling to torpor
#'
#' \code{coolEnergy} Calculate the energy required to cool to torpid temperatures
#'
#' @param Ta ambient temperature
#' @param bat.params list of bat parameters output from \code{\link{batLoad}}
#'
#' @return returns energy required to cool
#'
#' @example ExampleScripts/coolEnergy_ex.R
#'
#' @family Arousal Functions
#'
#' @seealso  \code{\link{arousalEnergy}}, \code{\link{coolTime}},
#' \code{\link{coolEnergy}}, \code{\link{euthermicEnergy}},
#' \code{\link{flyingEnergy}}, \code{\link{flyingTime}}
#'
#' @references McKechnie & Wolf 2004 equation 3
#'
#' @author Katie Haase
#' @export

coolEnergy<- function(Ta, bat.params){
  with(bat.params,{
    Q = 3.82-0.507*log10(Mass)
    ifelse(Ta > Ttormin,
           (TMRmin + (RMR*Q^((Ta-Teu)/10)))*((Teu-Ta)/((Ceu*(SA.body + SA.wing)*(Teu-Ta))/(0.1728*Mass))),
           (TMRmin + (RMR*Q^((Ttormin-Teu)/10)))*((Teu-Ttormin)/((Ceu*(SA.body + SA.wing)*(Teu-Ta))/(0.1728*Mass)))
    )

  })
}
