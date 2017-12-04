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
#' @export
DetModel <- function(t,y, params){
  with(c(as.list(y),params),{
    ttor <- CalcTorporTimePd(Ta = Tb, pct.rh = Hd, areaPd = FungalArea, WNS = WNS,
                           mod.params = params)
    tar <- CalcArousalTime(Ta = Tb, bat.params = params)
    tc <- CalcCoolTime(Ta = Tb, bat.params = params)
    tfl <- CalcTimeFlying( p = .999, bat.params = params)
    # change in TorporProp (pT)/dt
    dpTdt <- (pE/teu + pAr/tar + pC/tc + pFl/tfl)/4 - pT/ttor
    # change in ArousalProp (pAr)/dt
    dpAdt <- (pE/teu + pC/tc + pT/ttor + pFl/tfl)/4 - pAr/tar
    # change in CoolProp (pC)/dt
    dpCdt <- (pE/teu + pAr/tar + pT/ttor + pFl/tfl)/4 - pC/tc
    # change in EuthermicProp (pE)/dt
    dpEdt <- (pT/ttor + pAr/tar + pC/tc + pFl/tfl)/4 - pE/teu
    # Cange in FlyingProp (pF)/dt
    dpEfldt <- (pT/ttor + pAr/tar + pC/tc + pE/teu)/4 - pFl/tfl
    # change in EnergyConsumed/dt
    dJdt  <- Eeu*pE + Etor*pT + Ear*pAr + Ec*pC + Efl*pFl
    # change in precEArousal/dt
    dpJdt <- Eeu*pE +Ear*pAr + Ec*pC + Efl*pFl
    #change in FungalArea/dt
    dFdt  <- growth*pT

    list(c(dpTdt, dpEdt, dpEfldt, dpAdt, dpCdt, dJdt, dpJdt, dFdt))
  })
}

