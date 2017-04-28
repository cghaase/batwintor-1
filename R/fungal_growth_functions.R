#' How Q varries with ambient temperature
#'
#'
#' \code{CalcQ} Returns the the constant Q for the given ambient temperature.
#'
#' @param amb.temp ambient temperature in degrees C
#' @param kQ1 Q constant 1 = 1.6
#' @param kQ2 Q constant 2 = 0.26
#' @param kQ3 Q constant 3 = 0.006
#' @return Returns the value of Q for the given ambient temperature
#'
#' @examples
#' CalcQ(4)
#' CalcQ(6, 1.6, 0.26, 0.006)
CalcQ <- function( amb.temp, kQ1 = 1.6, kQ2 = 0.26, kQ3 = 0.006){
  kQ1 + kQ2*amb.temp - kQ3*amb.temp^2
}


#' How fungal growth rate varries with body temperature
#'
#'
#' \code{FungalGrowthRate} TODO : Need what actually occures
#'
#' @param params list of parameters passed through TODO NameFunction
#' @param body.temp body temperature of the animal in degrees C
#' @param t.min temperature minimum (= 0 degrees)
#' @return returns temperature determinate growth rate
#'
#' @example TODO
FungalGrowthRate <- function(body.temp, params, t.min = 0){
  with(params,{
    ifelse(body.temp > kBeta3|body.temp<=t.min,0,kBeta1*(body.temp-t.min)*
        (1-exp(kBeta2*(body.temp-kBeta3))))
  })
}


#' Scale fungal growth rate with relative humidity.
#'
#'
#' \code{ScaleFungalGrowthRate} Scales fungal growth rate with relative humidi
#' ty.
#'
#' @param params list of parameters passed through TODO NameFunction
#' @param pct.rh precent relative humidity
#' @return returns scaled humidity scaled fungal growth rate
#'
#' @example TODO
ScaleFungalGrowthRate <- function(params, pct.rh){
  with(params,{
    kMu1*pct.rh/(1+(kMu2*pct.rh))
  })
}
