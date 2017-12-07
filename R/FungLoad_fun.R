#' Load parameter sets for fungal growth
#'
#' \code{FungLoad} Loads fungal growth parameters from file and sets to selected option.
#'
#' @param path.to.params file path to the parameter files (without trailing "/")
#' @param growth.option fungal growth parameters see \strong{Details}
#'
#' @details Growth parameters are either: "Chaturvedi" (faster growth) from
#' chatruvedi et al. PLoS One, ot "Verant" (slower growth rate), from Verent et
#' al. PLoS One.
#'
#' \strong{mu1}: scaling parameter for Michaelis-Menton function
#'
#' \strong{mu2}: scaling parameter for Michaelis-Menton function
#'
#' \strong{beta1}: temperature dependant hourly rate shape parameter
#'
#' \strong{beta2}: temperature dependant hourly rate shape parameter
#'
#' \strong{beta3}: temperature dependant hourly rate shape parameter
#'
#' @note Requires both files to be in same directory with names
#' \code{rate.parms.csv} and \code{humid.parms.csv}. These files are
#' shipped with this package, however if you would like to alter these
#' paremeters you may do so through their associated \code{.csv} files.
#' @return Returns a named list of fungal growth scaling parameters
#'
#' @family Fungal funcitons
#' @seealso \code{\link{FungalGrowthRate}},  \code{\link{FungSelect}},
#' \code{\link{ScaleFungalGrowthRate}}
#' @example ExampleScripts/FungLoad_ex.R
#' @export
FungLoad <- function(path.to.params, growth.option){
  rate.pars <- read.csv(file.path(path.to.params,"rate.parms.csv"))
  if(growth.option%in%names(rate.pars)){
    r.pset <- subset(rate.pars, select = c("pars", growth.option))
  }else{
    warning("Unknown option selected for growth option")
  }
  scale.par <- read.csv(paste0(path.to.params,"humid.parms.csv"))
  colnames(scale.par) <- colnames(r.pset)
  pars <- rbind(r.pset,scale.par)
  growth.params <- as.list(pars[,2])
  names(growth.params) <- pars[,1]
  return(growth.params)
}
