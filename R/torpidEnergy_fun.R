#' Calculate energy per time at toprid body temp given ambient temperature
#'
#' \code{torpidEnergry} Function calculating the enegry required for a
#' bout of torpor at a ambient temperature
#'
#' @param Ta ambient emperature
#' @param bat.params list of bat parameters output from \code{\link{batLoad}}
#' @param q Q10 scaling value
#'
#' @return energy expended in volume \eqn{O^2}mL/h/g)
#'
#' @example ExampleScripts/torporEnergy_ex.R
#'
#' @family Torpor Functions
#'
#' @seealso \code{\link{ewl}},
#' @author Katie Haase
#' @export
torporEnergy <- function(Ta, bat.params, WNS, area, q = calcQ(Ta)){
  with(bat.params,{
    TMRminpd = TMRmin + (mrPd*(area/SA.wing))
    TMRmin = ifelse(WNS == TRUE, TMRminpd,TMRmin)
    ifelse(Ta > Ttormin, TMRmin*q^((Ta-Ttormin)/10),
           TMRmin + (Ttormin - Ta)*Ct)
  })
}
