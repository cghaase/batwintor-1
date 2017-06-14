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
    ttor <- CalcTorporTime(Ta = Ttor, areaPd = FungalArea, inf = WNS,
                           bat.params = params)
    tar <- CalcTorporTimePd()
    tc <- CalcCoolTime(Ta = Ttor, bat.params = params)
    dpTdt <- (pE/teu + pAr/tar + pC/tc)/3 - pT/ttor # change in TorporProp (pT)/dt
    dpAdt <- (pE/teu + pC/tc + pT/ttor)/3 - pAr/tar # change in ArousalProp (pAr)/dt
    dpCdt <- (pE/teu + pAr/tar + pT/ttor)/3 - pC/tc # change in CoolProp (pC)/dt
    dpEdt <- (pT/ttor + pAr/tar + pC/tc)/3 - pE/teu # change in EuthermicProp (pE)/dt
    dJdt  <- Eeu*pE + Etor*pT + Ear*pAr + Ec*pC # change in EnergyConsumed/dt
    dFdt  <- growth*pT #change in FungalArea/dt

    list(c(dpTdt, dpEdt, dJdt, dFdt))
  })
}
