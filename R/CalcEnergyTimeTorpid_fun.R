#' Calculate energy per time at toprid body temp given ambient temperature
#'
#' \code{CalcEnergyTimeTorpid} Function calculating the enegry required for a
#' bout of torpor at a ambient temperature
#'
#' @param Ta ambient emperature
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#' @param q Q10 scaling value
#'
#' @return TODO find units accociated with
#'
#' @example ExampleScripts/CalcEnergyTimeTorpid_ex.R
#'
#' @author Katie Haase
CalcEnergyTimeTorpid <- function(Ta, bat.params, q = CalcQ(Ta)){
  with(bat.params,{
    ifelse(Ta > Ttormin, TMRmin*q^((Ta-Ttormin)/10),
           TMRmin + (Ttormin - Ta)*Ct)
  })
}
