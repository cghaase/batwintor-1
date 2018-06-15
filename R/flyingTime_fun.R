#' Calculate the time spent flying
#'
#' \code{flyingTime} calculates the time spent flying as a proportion of
#' time in euthermic arrousals
#' @param bat.params see \code{\link{batLoad}}
#'
#' @return a time
#' @family Arousal Functions
#'
#' @seealso  \code{\link{arousalEnergy}}, \code{\link{coolTime}},
#' \code{\link{coolEnergy}}, \code{\link{euthermicEnergy}},
#' \code{\link{flyingEnergy}}
#' @export
flyingTime <- function(bat.params){
  with(bat.params,{
    pFly*teu
  })
}
