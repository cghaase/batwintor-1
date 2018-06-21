#' Calculate maximum time in torpor
#'
#' \code{torporTime} Calculates time or torpor bout given ambient temperature or EWL, whichever comes first
#'
#' @param Ta ambient temperature
#' @param pct.rh precent humidity
#' @param areaPd surface area infected by fungus
#' @param WNS logical, if TRUE, considers the effect of Pd growth on EWL
#' @param bat.params  parameters returned by \code{bat.params}
#' @param fung.params parameters returned by \code{fung.params}
#'
#' @export

torporTime <- function(Ta, pct.rh, areaPd, WNS, bat.params, fung.params){
  mod.params <- as.list(c(bat.params, fung.params))
  with(mod.params,{
    #Define threshold based on infection status
    Hd = ifelse(pct.rh==100,99,pct.rh)
    pO2     = 0.2095      #volumetric proportion of oxygen in air
    O2.coef = 0.15        #coefficient of oxygen extraction efficiency from air for bat's respiratory system
    a  = 0.611            #constants
    b  = 17.502
    c  = 240.97
    GC = 0.0821           #universal gas constant
    Q=calcQ(Ta)
    mrPd = 1.4
    aPd = 0.16

    #Calculate water vapor pressure differential
    WVP.skin <- ifelse(Ta > Ttormin,
                       (a * exp((b * Ta)/(Ta + c))),
                       (a * exp((b * Ttormin)/(Ttormin + c))))

    WVP.air <- (Hd*0.01) * (a * exp((b * Ta)/(Ta + c)))

    dWVP    <- WVP.skin - WVP.air

    #Calculate saturation deficiet
    mgL.skin <- ifelse(Ta > Ttormin,
                       ((WVP.skin * 0.00986923)/(GC * (Ta + 273.15)))*18015.28,           #convert kPa to atm; C to Kelvin; moles to mg
                       ((WVP.skin * 0.00986923)/(GC * (Ttormin + 273.15)))*18015.28)

    mgL.air <- (((Hd *0.01) * WVP.air * 0.00986923)/(GC * (Ta + 273.15)))*18015.28    #convert Hd to fraction; convert kPa to atm; C to Kelvin; moles to mg

    sat.def <- mgL.skin - mgL.air

    #Calculate cutaneous EWL (mg/hr) from the wing surface
    cEWL.body <- SA.body * rEWL.body * dWVP
    cEWL.wing <- SA.wing * rEWL.wing * dWVP
    cEWL = cEWL.body + cEWL.wing

    #Calculate pulmonary EWL (mg/hr)
    vol  <- (as.numeric(TMRmin) * Mass)/(pO2 * O2.coef * 1000) #1000 used to convert L to ml
    pEWL <- vol * sat.def

    #Calculate total EWL (TEWL; mg/hr)
    TEWL <- cEWL + pEWL

    #Calculate % of body mass (in mg) and compare to TEWL
    threshold <- (pLean*Mass*1000)*pMass

    #Calculate how long until threshold is reached
    EWL.time <- threshold/TEWL

    #Calculate torpor time as a function of Ta (without EWL)
    Ta.time <- ifelse(Ta > Ttormin,
                      ttormax/Q^((Ta-Ttormin)/10),
                      ttormax/(1+(Ttormin-Ta)*Ct/(TMRmin)))

    if(WNS == FALSE){
      return(ifelse(Ta.time < EWL.time, Ta.time, EWL.time))
    } else if(WNS == TRUE){
      p.areaPd = areaPd/SA.wing*100                   #proportion of wing surface area covered in Pd growth

      #Calculate cutaneous EWL (mg/hr) from wing surface area
      cEWL.body.pd <- SA.body * rEWL.body * dWVP
      cEWL.wing.pd <- SA.wing * (rEWL.wing+(0.16*p.areaPd)) * dWVP
      cEWL.pd = cEWL.body.pd + cEWL.wing.pd

      #Calculate pulmonary EWL (mg/hr) with increased TMR?
      vol.pd  <- ((TMRmin + (mrPd*p.areaPd)) * Mass)/(pO2 * O2.coef * 1000) #1000 used to convert L to ml
      pEWL.pd <- vol.pd * sat.def

      #Calculate total EWL (TEWL; mg/hr)
      TEWL.pd <- cEWL.pd + pEWL.pd

      #In Liam's paper, the relationship between EWL and Pd growth was EWL = (p.areaPd*0.21) + 12.80
      #So, when area = 0 (no Pd), EWL for this temp/humidity was 12.8.
      #Therefore the addition of 0.21*area would be added to whatever the EWL was at any temp/humdity relationship as both temp/humidity are included in Pd growth estimates?
      threshold.pd <- (pLean*Mass*1000)*pMass.i
      Pd.time <- threshold.pd/TEWL.pd

      #Calculate torpor time as a function of Ta (without EWL) with increased TMR
      Ta.time.pd <- ifelse(Ta > Ttormin,
                           ttormax/Q^((Ta-Ttormin)/10),
<<<<<<< HEAD
                           ttormax/(1+(Ttormin-Ta)*Ct/(TMRmin+(mrPd*p.areaPd))))
=======
                           ttormax/(1+(Ttormin-Ta)*Ct/TMRmin+(mrPd*p.areaPd)))
>>>>>>> R$/Testing

      return(ifelse(Ta.time.pd < Pd.time, Ta.time.pd, Pd.time))
    }
  })
}
