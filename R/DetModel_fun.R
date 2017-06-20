#' Deterministic Model of Fungal Growth
#'
#' \code{DetModel} solves differential equasions accociated with the
#' deterministic model of fungal growth.
#'
#' @param t tmeperature I guess? does it even get used? TODO
#' @param y dependant varriables calculated within
#' \code{\link{DynamicEnergyPd}}
#' @param params parameters passed through \code{\link{DynamicEnergyPd}}
#'
#' @details This may have room for improvement in it and may change in the
#' future
#'
#' @seealso \code{\link{DynamicEnergyPd}}
#' @example ExampleScripts/DynamicEnergyPd_ex.R
#'
DetModel <- function(t,y, params){
  require(deSolve)
  with(c(as.list(y),params),{
    ttor <- CalcTorporTime(Ta = Tb, areaPd = FungalArea, inf = WNS,
                           bat.params = params)
    tar <- CalcArousalTime(Ta = Tb, bat.params = params)
    tc <- CalcCoolTime(Ta = Tb, bat.params = params)
    # change in TorporProp (pT)/dt
    dpTdt <- (pE/teu + pAr/tar + pC/tc)/3 - pT/ttor
    # change in ArousalProp (pAr)/dt
    dpAdt <- (pE/teu + pC/tc + pT/ttor)/3 - pAr/tar
    # change in CoolProp (pC)/dt
    dpCdt <- (pE/teu + pAr/tar + pT/ttor)/3 - pC/tc
    # change in EuthermicProp (pE)/dt
    dpEdt <- (pT/ttor + pAr/tar + pC/tc)/3 - pE/teu
    # change in EnergyConsumed/dt
    dJdt  <- Eeu*pE + Etor*pT + Ear*pAr + Ec*pC
    # change in precEArousal/dt
    dpJdt <- Eeu*pE +Ear*pAr + Ec*pC
    #change in FungalArea/dt
    dFdt  <- growth*pT

    list(c(dpTdt, dpEdt, dpAdt, dpCdt, dJdt, dpJdt, dFdt))
  })
}
