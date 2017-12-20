#' Scale fungal growth rate with relative humidity.
#'
#' \code{scaleFungalGrowth} scales fungal growth rate with relatives
#' humidity.
#'
#' @param fung.params list of parameters passed through \code{\link{FungLoad}}
#' @param pct.rh precent relative humidity
#' @return returns scaled humidity scaled fungal growth rate
#'
#' @family Fungal Functions
#' @seealso \code{\link{fungalLoad}}, \code{\link{fungalSelect}},
#' \code{\link{fungalGrowth}}
#' @example ExampleScripts/scaleFungalGrowth_ex.R
#' @export
scaleFungalGrowth <- function(pct.rh, fung.params){
  with(fung.params,{
    mu1*pct.rh/(1+(mu2*pct.rh))
  })
}
