#' Load static bat metabolic parameters
#'
#' \code{BatLoad} Loads metabolic parameters for bats from dataframe.
#'
#' @param x dataframe from which parameters are taken see \strong{Details}
#' @param species character string for selected species from file above
#' @return returns list of names parameters used for metabolic calculations
#'
#' @details Function loads the metabolic parameters that will be used accross
#' the entire distribution. Generally speaking it is suggested that you use
#' \code{\link{bat.params}} which is built in.
#'
#' @example ExampleScripts/BatLoad_ex.R
#' @export

BatLoad <- function(x, species){
  if(species%in%rownames(x)){
    pset <- x[species,]
  }else{
    warning("Unknown species option selected")
  }
  par <- as.list(pset)
  return(par)
}
