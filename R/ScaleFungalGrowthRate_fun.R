#' Scale fungal growth rate with relative humidity.
#'
#' \code{ScaleFungalGrowthRate} scales fungal growth rate with relative
#' humidity.
#'
#' @param fung.params list of parameters passed through \code{\link{FungLoad}}
#' @param pct.rh precent relative humidity
#' @return returns scaled humidity scaled fungal growth rate
#'
#' @family Fungal Functions
#' @seealso \code{\link{FungLoad}}, \code{\link{FungSelect}},
#' \code{\link{FungalGrowthRate}}
#' @example ExampleScripts/ScaleFungalGrowthRate_ex.R
#' @export
ScaleFungalGrowthRate <- function(pct.rh, fung.params){
  with(fung.params,{
    mu1*pct.rh/(1+(mu2*pct.rh))
  })
}
