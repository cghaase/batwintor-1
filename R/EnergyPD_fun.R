#' Calculate energy use and fungal area as a function of environmental
#' condtions and infection status
#'
#' \code{EnergyPd} calculates energy consumed and the area of funal growth
#' given static environmental conditions across a predefined length of winter.
#'
#' @param Ta ambient temperature
#' @param twinter length of winter
#' @param Hd humidity
#' @param bat.prams parameters returned by \code{\link{LoadBat}}
#' @param fung.params parameters returned by \code{\link{LoadFung}}
#'
#' @details TODO
#' Does this function need to exist?
#' @examples TODO
EnergyPd <- function(Ta, Hd, SA.type = c("wing", "body"), pmass = 0.043, bat.params, fung.params, Q=calcQ(Ta), WNS = c("TRUE, FALSE")){  
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
    Ecool <- CalcEnergyCool(Ttor, mod.params)
    tar <- CalcArousalTime(Ta, bat.params)
    tcool <- CalcCoolTime(Ta, bat.params)
    ttor <- CalcTorporTimePd(Ta, Hd, SA.type, WNS, pmass, Q=calcQ(Ta), mod.params)
    Ebout <- Eeu*teu + Etor*ttor + Ear + Ecool
    Ewinter <- (twinter/(ttor + teu + tar + tcool))*Ebout
    fatConsumed <- ConvertToFat(Ewinter)
    results <- c(grams=fatConsumed, area=Pd, time.tor=ttor)
    return(results)
  })
}
