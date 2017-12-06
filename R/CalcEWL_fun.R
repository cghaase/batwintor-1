#' Calculate Evaporative Water Loss
#'
#' \code{CalcEWL} calculates evaporative waterloss based on species specific parameter.
#' @param Ta ambient temperature (degrees C)
#' @param pct.rh precent relative humidity
#' @param t time
#' @param areaPd area of fungal growth (cm2)
#' @param mod.params list of parameters output from \code{\link{BatLoad}}
#'  and \code{\link{FungLoad}}
#' @param torpid logical, if the animal is storpid or not
#' @param WNS logical, if the animal is infected with Pd or not
#'
#' @details The function includes mechanisms to deal with the evaporative water loss (EWL)
#' of both torpid and euthermic individuals, as well as increasing metabolic rates
#' for those infected with Pd.
#' @export
CalcEWL <- function(Ta, pct.rh, t, areaPd, mod.params = c(fung.params,bat.params),
                    torpid = TRUE, WNS = c(TRUE,FALSE)){
  with(mod.params,{

    #### Constants ####
    pO2     = 0.2095      #volumetric proportion of oxygen in air
    O2.coef = 0.15        #coefficient of o2 extraction efficiency
    a  = 0.611            #constants
    b  = 17.502
    c  = 240.97
    GC = 0.0821           #universal gas constant
    k  = 10               #Meeh factor

    #### Surface area ####
    SA <- k*(mass^(2/3))

    #### Water vapor presure ####
    #Calculate water vapor pressure differential
    if(torpid == TRUE){
      WVP.skin <- ifelse(Ta > Ttormin,
                         (a * exp((b * Ta)/(Ta + c))),
                         (a * exp((b * Ttormin)/(Ttormin + c))))
    } else{
      WVP.skin <- ifelse(Ta > Tlc,
                         (a * exp((b * Teu)/(Teu + c))),
                         (a * exp((b * Tlc)/(Tlc + c))))
    }

    WVP.air <- (pct.rh*0.01) * (a * exp((b * Ta)/(Ta + c)))

    dWVP    <- WVP.skin - WVP.air

    #### Saturation deficiet ####
    if(torpid == TRUE){
      mgL.skin <- ifelse(Ta > Ttormin,
                         #convert kPa to atm; C to Kelvin; moles to mg
                         ((WVP.skin * 0.00986923)/(GC * (Ta + 273.15)))*18015.28,
                         ((WVP.skin * 0.00986923)/(GC * (Ttormin + 273.15)))*18015.28)
    } else{
      mgL.skin <- ifelse(Ta > Tlc,
                         #convert kPa to atm; C to Kelvin; moles to mg
                         ((WVP.skin * 0.00986923)/(GC * (Teu + 273.15)))*18015.28,
                         ((WVP.skin * 0.00986923)/(GC * (Tlc + 273.15)))*18015.28)
    }
    #convert pct.rh to fraction; convert kPa to atm; C to Kelvin; moles to mg
    mgL.air <- (((pct.rh *0.01) * WVP.air * 0.00986923)/(GC * (Ttormin + 273.15)))*18015.28

    sat.def <- mgL.skin - mgL.air

    #### EWL ####
    #Calculate cutaneous EWL (mg/hr)
    cEWL <- SA * rEWL * t * dWVP

    #Calculate pulmonary EWL (mg/hr)
    vol <- ifelse(torpid == TRUE,
                  (TMRmin * t * mass)/(pO2 * O2.coef * 1000),
                  (RMR * t * mass)/(pO2 * O2.coef * 1000))
    pEWL <- vol * sat.def

    #Calculate total EWL (TEWL; mg/hr)
    TEWL <- cEWL + pEWL

    #### Infection ####
    if(WNS == TRUE){

      #Calculate new growth of Pd for hour
      #proportion of wing surface area covered in Pd growth
      p.areaPd = areaPd/SA*100

      #Calculate cutaneous EWL (mg/hr)
      cEWL.pd <- SA * (rEWL*rPd) * t * dWVP

      #Calculate pulmonary EWL (mg/hr) with increased TMR?
      vol.pd <- ifelse(torpid == TRUE,
                       (TMRmin * t * mass)/(pO2 * O2.coef * 1000),
                       (RMR * t * mass)/(pO2 * O2.coef * 1000))
      pEWL.pd <- vol.pd * sat.def

      #Calculate total EWL (TEWL; mg/hr)
      TEWL.pd <- cEWL.pd + pEWL.pd

      #Add increase associated with Pd growth
      TEWL.pd <- TEWL.pd + (p.areaPd*aPd)

      return(data.frame(Ta = Ta, pct.rh = pct.rh, TotalEWL = TEWL.pd, CutaneousEWL = cEWL.pd,
                        PulmonaryEWL = pEWL.pd, PercPdGrowth = p.areaPd))
    } else{
      return(data.frame(Ta = Ta, pct.rh = pct.rh, TotalEWL = TEWL, CutaneousEWL = cEWL,
                        PulmonaryEWL = pEWL))
    }
  })
}
