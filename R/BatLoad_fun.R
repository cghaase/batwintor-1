#' Load static bat metabolic parameters
#'
#' \code{BatLoad} Loads static metabolic parameters for bats from dataframe.
#'
#' @param x dataframe from which parameters are taken see \strong{Details}
#' @param species character string for selected species from file above
#' @return returns list of names parameters used for metabolic calculations
#'
#' @details Function loads the metabolic parameters that will be used accross
#' the entire distribution.
#'
#' \strong{RMR}: resting metabolic rate
#'
#' \strong{TMR.min}: torpor metabolic rate
#'
#' \strong{Teu}: temperature euthermic
#'
#' \strong{Tlc}: lower critical temperature during euthermia
#'
#' \strong{Ttormin}: temperature where \code{tmr.min} is achieved
#'
#' \strong{Ceu}: euthermic thermal conductance
#'
#' \strong{Ct}: thermal conductance during torpor
#'
#' \strong{S}: specific heat capacity of tissue
#'
#' \strong{k}: some constant
#'
#' \strong{ttormax}: maximum time spent in torpor
#'
#' \strong{tar}: time to arousal
#'
#' \strong{teu}: time spent euthermic during arousals
#'
#' \strong{mass}: mass of the critter
#'
#' @example ExampleScripts/BatLoad_ex.R

BatLoad <- function(x, species){
  if(species%in%rownames(x)){
    pset <- x[species,]
  }else{
    warning("Unknown species option selected")
  }
  par <- as.list(pset)
  return(par)
}
