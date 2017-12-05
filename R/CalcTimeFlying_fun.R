#' Calculate the time spent flying
#'
#' \code{calcTimeFlying} calculates the time spent flying as a proportion of
#' time in euthermic arrousal
#' @param pFly proportion of euthermia spent flying
#' @param bat.params see \code{\link{BatLoad}}
#'
#' @return a time
#' @export
CalcTimeFlying <- function(pFly, bat.params){
  with(bat.params,{
    pFly*teu
  })
}
