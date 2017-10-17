#' How fungal growth rate varries with body temperature
#'
#' \code{FungalGrowthRate} Scales chosen fungal growth rate with to the
#' body temperautre of the infected animal.
#'
#' @param fung.params list of parameters passed through TODO NameFunction
#' @param Tb body temperature of the animal in degrees C
#' @param t.min temperature minimum (= 0 degrees C)
#' @return returns temperature determinate growth rate
#'
#' @seealso \code{\link{FungLoad}}
#' @example ExampleScripts/FungalGrowthRate_ex.R
#' @export
FungalGrowthRate <- function(Tb, fung.params, t.min = 0){
  with(fung.params,{
    ifelse(Tb > beta3|Tb<=t.min,0,beta1*(Tb-t.min)*
             (1-exp(beta2*(Tb-beta3))))
  })
}
