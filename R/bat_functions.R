#' Load static bat metabolic parameters
#'
#' \code{BatLoad} Loads static metabolic parameters for bats from dataframe.
#'
#' @param x dataframe from which parameters are taken see\strong(bold){Details}
#' @param species character string for selected species from file above
#' @return returns list of names parameters used for metabolic calculations
#'
#' @details Function loads the metabolic parameters that will be used accross
#' the entire distribution, futrure versions will undate this function.
#' \emph{italics}{rmr}: resting metabolic rate
#' \emph{italics}{tmr.min}: torpor metabolic rate
#' \emph{italics}{t.eu}: temperature euthermic
#' \emph{italics}{t.lc}: lower critical temperature during euthermia
#' \emph{italics}{t.tor.min}: temperature where \emph{italics}{tmr.min} is achieved
#' \emph{italics}{c.eu}: euthermic thermal conductance
#' \emph{italics}{c.t}: thermal conductance during torpor
#' \emph{italics}{s}: specific heat capacity of tissue
#' \emph{italics}{k}: some constant
#' \emph{italics}{ti.tor.max}: maximum time spent in torpor
#' \emph{italics}{ti.ar}: time to arousal
#' \emph{italics}{ti.eu}: time spent euthermic during arousals
#' \emph{italics}{mass}: mass of the critter

BatLoad <- function(x, species){
  if(species%in%names(x)){
    pset <- subset(x, select = c("Parameter", species))
  }else{
    warning("Unknown species option selected")
  }
  par <- as.list(pset[,2])
  names(par) <- pset[,1]
  return(par)
}

#' How Q varries with ambient temperature
#'
#'
#' \code{CalcQ} Returns the the constant Q for the given ambient temperature.
#'
#' @param t.amb ambient temperature in degrees C
#' @param kQ1 Q constant 1 = 1.6
#' @param kQ2 Q constant 2 = 0.26
#' @param kQ3 Q constant 3 = 0.006
#' @return Returns the value of Q for the given ambient temperature
#'
#' @examples
#' CalcQ(4)
#' CalcQ(6, 1.6, 0.26, 0.006)
CalcQ <- function( t.amb, kQ1 = 1.6, kQ2 = 0.26, kQ3 = 0.006){
  kQ1 + kQ2*t.amb - kQ3*t.amb^2
}


#' Calculate engegry per time euthermia with ambient temperature
#'
#' \code{CalcEnergyTimeEuthermic} Function calculating the enegry required to ma
#' intain euthermia for a given time and ambient temperature.
#'
#' @param amb.tmp ambient temperature
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#'
#' @return returns stuff TODO
CalcEnergyTimeEuthermic <- function(t.amb, bat.params){
  with(bat.params,{
    rmr + (t.lc-t.amb)*c.eu
  })}

#' Calculate energy per time at toprid body temp given ambient temperature
#'
#' \code{CalcEnegryTimeTorpid} Function calculating the enegry required for a bo
#' ut of torpor at a ambient temperature
#'
#' @param t.amb ambient emperature
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#' @param q what ever Q is TODO
#'
#' @return returns enegry reqired TODO
CalcEnegryTimeTorpid <- function(t.amb, bat.params, q = CalcQ(t.amb)){
  with(bat.params,{
    ifelse(t.amb > t.tor.min, tmr.min*q^((t.amb-t.tor.min)/10),
      tmr.min + (t.tor.min - t.amb)*c.t)
    })
}

#' Calculate enegry needed during an arousal from torpor
#'
#' \code{CalcEnegryArousal} Calculate the enegry required to maintai
#' n eurthermia during an arousal from torpor.
#'
#' @param t.topid
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#'
#' @return returns enegry required to arouse
#'
CalcEnegryArousal <- function(t.tor, bat.params){
  with(bat.params,{
    (t.eu - t.tor) * s
  })
}

#' Calculate time in torpor with respect to temperature
#'
#' \code{CalcTorporTime} Calculate time of a torpor bout given ambient temperat
#' ure and fungal growth area
#'
#' @param t.amb ambient temperature
#' @param area.pd area of pd
#' @param inf infection status (TRUE or FALSE)
#' @param bat.params list of bat parameters output from \code{\link{BatLoad}}
#' @param q what ever Q is TODO
#'
#' @return returns time bat spends in torpor
CalcTorporTime <- function(t.amb, area.pd, inf, bat.params, q=CalcQ(t.amb)){
  with(bat.params,{
    ti.tor <- ifelse(t.amb > t.tor.min,
      (ti.tor.max/q^((t.amb - t.tor.min)/10)),
      ti.tor.max/(1+(t.tor.min - t.amb) * c.t/tmr.min))
    if(inf==TRUE){
      area.pd <- ifelse(area.pd < 1, 1, area.pd)
      ti.tor <- ti.tor/area.pd # how fungal growth reduceds time torpid
    }
    return(ti.tor)
  })
}
