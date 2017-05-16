#' Deterministic Model of Fungal Growth
#'
#' \code{DetModel} solves differential equasions accociated with the
#' deterministic model of fungal growth.
#'
#' @param t tmeperature I guess? does it even get used? TODO
#' @param y dependant varriables calculated within
#' \code{\link{DynamicEnegryPd}}
#' @param params parameters passed through \code{\link{DynamicEnegryPd}}
#'
#' @details This may have room for improvement in it and may change in the
#' future
#'
#' @seealso \code{\link{DynamicEnegryPd}}
#'
#'
DetModel <- function(t,y, params){
  require(deSolve)
  with(c(as.list(y),params),{
    ttor <- CalcTorporTime(Ta = Ttor, areaPd = FungalArea, inf = WNS,
                           bat.params = params)
    dpTdt <- pE/tue - pT/ttor # change in TorporProp (pT)/dt
    dpEdt <- pT/ttor - pE/teu # change in EuthermicProp (pE)/dt
    dJdt  <- Eeu*pE + Etor*pT + Ear*pT/ttor # change in EnegryConsumed/dt
    dFdt  <- growth*pT #change in FungalArea/dt

    list(c(dpTdt, dpEdt, dJdt, dFdt))
  })
}
