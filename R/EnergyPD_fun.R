#' Calculate energy use and fungal area as a function of environmental
#' condtions and infection status
#'
#' \code{EnergyPd} calculates energy consumed and the area of funal growth
#' given static environmental conditions across a predefined length of winter.
#'
#' @param Ta ambient temperature
#' @param twinter length of winter
#' @param Hd humidity
#' @param WNS logical, should the bats have WNS?
#' @param bat.params parameters returned by \code{\link{BatLoad}}
#' @param fung.params parameters returned by \code{\link{FungLoad}}
#'
#' @export
EnergyPd <- function(Ta, twinter, Hd, WNS, bat.params, fung.params){
  mod.params <- as.list(c(bat.params, fung.params))
  with(mod.params,{
    Ttor <- ifelse(Ta > Ttormin, Ta, Ttormin)
    Tb <- ifelse(Ttor < Teu,Ttor,Teu)
    # Fungal Growth
    growthrate <- FungalGrowthRate(Tb, mod.params)
    growthrate.H <- ScaleFungalGrowthRate(Hd, mod.params)
    PD <- growthrate*growthrate.H
    # Energy use
    Eeu <- CalcEnergyTimeEuthermic(Ta, mod.params)
    Etor <- CalcEnergyTimeTorpid(Ta, mod.params)
    Ear <- CalcEnergyArousal(Ttor, mod.params)
    ttor <- CalcTorporTime(Ta, Pd, WNS, mod.params)
    Ebout <- Eeu*teu + Etor*ttor + Ear*tar
    Ewinter <- (twinter/(ttor + teu + tar))*Ebout
    fatConsumed <- ConvertToFat(Ewinter)
    results <- c(grams=fatConsumed, area=Pd, time.tor=ttor)
    return(results)
  })
}
