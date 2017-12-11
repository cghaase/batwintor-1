#' Calculate time inactive during euthermia
#'
#' \code{CalcTimeEuthermic}
#'
#' @param bat.params see \code{\link{BatLoad}}
#'
#' @return returns the time spent in euthermia (but not flying)
#' @export
CalcTimeEuthermic <- function(bat.params){
  with(bat.params,{
    (1-pFly)*teu
  })
}
