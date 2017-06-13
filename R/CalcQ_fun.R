#' Q10 scaling value as a function of ambient temperature
#'
#' \code{CalcQ} Scales metabolic rate with ambient temperature
#'
#' @param Ta ambient temperature in degrees C
#' @param Q1 Q constant 1 = 1.6
#' @param Q2 Q constant 2 = 0.26
#' @param Q3 Q constant 3 = 0.006
#' @return Returns the value of Q10 value for the given ambient temperature
#'
#' @details TODO Q10 fit source needed
#'
#' TODO Doesn't this only apply down untill Tlc?
#' @examples
#' CalcQ(4)
#' CalcQ(6, 1.6, 0.26, 0.006)
CalcQ <- function( Ta, Q1 = 1.6, Q2 = 0.26, Q3 = 0.006){
  Q1 + Q2*Ta - Q3*Ta^2
}

#TEST 3
