#' How fungal growth rate varries with body temperature
#'
#' \code{fungalGrowth} Scales chosen fungal growth rate with to the
#' body temperautre of the infected animal.
#'
#' @param fung.params list of parameters passed through TODO NameFunction
#' @param Tb body temperature of the animal in degrees C
#' @param t.min temperature minimum (= 0 degrees C)
#' @return returns temperature determinate growth rate
#'
#' @family Fungal functions
#' @seealso \code{\link{fungalLoad}},  \code{\link{fungalSelect}},
#' \code{\link{scaleFungalGrowth}}
#' @example ExampleScripts/fungalGrowth_ex.R
#' @export
fungalGrowth <- function(Tb, fung.params, t.min = 0){
  with(fung.params,{
    ifelse(Tb > beta3|Tb<=t.min,0,beta1*(Tb-t.min)*
             (1-exp(beta2*(Tb-beta3))))
  })
}
