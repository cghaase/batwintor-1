#' Load parameter sets for fungal growth
#'
#' \code{FungLoad} Loads fungal growth parameters and sets to selected option.
#'
#' @param path.to.params file path to the parameter files (with "//")
#' @param growth.option fungal growth parameters see \strong(bold){Details}
#'
#' @details Growth parameters are either: "Chaturvedi" (faster growth) from
#' chatruvedi et al. PLoS One, ot "Verant" (slower growth rate), from Verent et
#' al. PLoS One.
#' \emph{italics}{mu1}: scaling parameter for Michaelis-Menton function
#' \emph{italics}{mu2}: scaling parameter for Michaelis-Menton function
#' \emph{italics}{beta1}: temperature dependant hourly rate shape parameter
#' \emph{italics}{beta2}: temperature dependant hourly rate shape parameter
#' \emph{italics}{beta3}: temperature dependant hourly rate shape parameter
#'
#' @return Returns a names list of fungal growth scaling parameters
#'
FungLoad <- function(path.to.params, growth.option){
  rate.pars <- read.csv(paste0(path.to.params,"rate.parms.csv"))
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

#' How fungal growth rate varries with body temperature
#'
#'
#' \code{FungalGrowthRate} TODO : Need what actually occures
#'
#' @param fung.params list of parameters passed through TODO NameFunction
#' @param t.body body temperature of the animal in degrees C
#' @param t.min temperature minimum (= 0 degrees)
#' @return returns temperature determinate growth rate
#'
#' @example TODO
FungalGrowthRate <- function(t.body, fung.params, t.min = 0){
  with(fung.params,{
    ifelse(t.body > beta3|t.body<=t.min,0,beta1*(t.body-t.min)*
        (1-exp(beta2*(t.body-beta3))))
  })
}


#' Scale fungal growth rate with relative humidity.
#'
#'
#' \code{ScaleFungalGrowthRate} Scales fungal growth rate with relative humidi
#' ty.
#'
#' @param fung.params list of parameters passed through TODO NameFunction
#' @param pct.rh precent relative humidity
#' @return returns scaled humidity scaled fungal growth rate
#'
#' @example TODO
ScaleFungalGrowthRate <- function(fung.params, pct.rh){
  with(fung.params,{
    mu1*pct.rh/(1+(mu2*pct.rh))
  })
}
