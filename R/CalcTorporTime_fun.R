#' Calculate maximum time in torpor
#'
#' \code{calcTorporTime} Calculates time or torpor bout given ambient temperature or EWL, whichever comes first
#'
#' @param Ta ambient temperature
#' @param Hd percent humidity
#' @param SA.type surface area measurement type, either `wing` or `body`
#' @param pmass propotion of body mass (in mg) to designate EWL threshold; default is 4.3%
#' @param params list of bat parameters output from \code{\link{BatLoad}}
#' @param Q change in torpor metabolism as a function of Ta
#'
#' @details TODO
#' @examples TODO

calcTorporTime <- function(Ta, Hd, SA.type = c("body", "wing"), pmass = 0.043, params, Q=calcQ(Ta)){
  with(as.list(params),{
    
    #Known constants 
    pO2     = 0.2095      #volumetri proportion of oxygen in air 
    O2.coef = 0.30        #coefficient of oxygen extraction efficiency from air for bat's respiratory system
    a  = 0.611            #constants
    b  = 17.502
    c  = 240.97
    GC = 0.0821           #universal gas constant
    k  = 10               #Meeh factor
    #pmass = 0.043  
    
    #Calculate surface area
    SA.body <-  k * (mass^(2/3))
    SA.wing <- (k * (mass^(2/3))) + wing.area
    SA <- ifelse(SA.type == "wing", SA.wing, SA.body)
    
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
    
    mgL.air <- (((Hd *0.01) * WVP.air * 0.00986923)/(GC * (Ttormin + 273.15)))*18015.28    #convert Hd to fraction; convert kPa to atm; C to Kelvin; moles to mg
    
    sat.def <- mgL.skin - mgL.air
    
    #Calculate cutaneous EWL (mg/hr)
    cEWL <- SA * rEWL * dWVP                          
    
    #Calculate pulmonary EWL (mg/hr)
    vol  <- ((TMRmin * mass)/(pO2 * O2.coef * 1000)) #1000 used to convert L to ml
    pEWL <- vol * sat.def                      
    
    #Calculate total EWL (TEWL; mg/hr)
    TEWL <- cEWL + pEWL
    
    #Calculate % of body mass (in mg) and compare to TEWL
    threshold <- (mass*1000)*pmass
    
    #Calculate how long until threshold is reached
    EWL.time <- threshold/TEWL
    
    #Calculate torpor time as a function of Ta (without EWL)
    Ta.time <- ifelse(Ta > Ttormin, 
                      ttormax/Q^((Ta-Ttormin)/10), 
                      ttormax/(1+(Ttormin-Ta)*Ct/TMRmin))
    
    #Return the shortest torpor time; assuming that if EWL threshold hasn't been met, torpor duration will be a function of Ta
    return(ifelse(Ta.time < EWL.time, Ta.time, EWL.time))
  }
  )
}