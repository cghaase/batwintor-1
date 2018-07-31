#' Calculate the energy expended during flight
#'
#' \code{flyingEnergy} calculates energry expended during a unit of light
#' @param Ta ambient tempterature
#' @param bat.params see \code{\link{batLoad}}
#'
#' @return energy expended during a time unit of flight

#' @family Arousal Functions
#'
#' @seealso  \code{\link{arousalEnergy}}, \code{\link{coolTime}},
#' \code{\link{coolEnergy}}, \code{\link{euthermicTime}},
#' \code{\link{euthermicEnergy}},
#' \code{\link{flyingEnergy}}, \code{\link{flyingTime}}
#' @export
flyingEnergy <- function(Ta, bat.params){
  with(bat.params,{
    Tb = ifelse(Ta<=Ttormin,Ttormin,Ta)
    Pf = -0.638 + (0.808*log10(Mass))                 #Energetic cost of flight (in Watts)
    Kfur = 4.154 + (1.3*(3.7^1))                      #Forced convective coefficient
    Tsky = Ta - 20.4 + (0.22*Ta)                      #temperature of atmospheric sky
    esky = 0.398 * (10^-5) * ((Ta + 273.15)^2.148)    #emissivity of sky
    #Calculate different costs per body part
    Efly_wing = (44.27*(Tb-Ta)) + (5*10^-8)*0.98*esky*(((Tb+273.15)^4)-((Tsky+273.15)^4))*SA.wing
    Efly_body = (Kfur*(Teu-Ta)) + (5*10^-8)*0.98*esky*(((Teu+273.15)^4)-((Tsky+273.15)^4))*SA.body
    Efly_all  = 0.2*Pf + Efly_body + Efly_wing
    Efl = (((Efly_all*0.0143*60)/5.05))/Mass       #Convert from Watts to ml O2 per g
    return(Efl)
  })}
