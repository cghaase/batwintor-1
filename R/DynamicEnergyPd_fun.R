#' Dynamic energy cost calculation
#'
#' \code{DynamicEnergyPd} determines the growth area of Pd and the sums the
#' energy consumed under specified environmental conditions.
#'
#' @param env.df dataframe conataing range of env conditions
#' @param inf infection status (\code{TRUE} or \code{FALSE})
#' @param bat.params parameters returned by \code{\link{BatLoad}}
#' @param fung.params parameters returned by \code{\link{FungLoad}})
#'
#' @details TODO
#' @seealso \code{\link{DetModel}}
#' @example ExampleScripts/DynamicEnergyPd_ex.R
#'
DynamicEnergyPd <- function(env.df, inf, bat.params, fung.params){
  require(deSolve); require(data.table)
  mod.params <- as.list(c(bat.params, fung.params))
  with(mod.params,{
    Ta <- env.df[[1]]
    Hd <- env.df[[2]]
    if(beta3 >= Teu){
      warning("The model assumes fungal growth does not occure at euthermic
              temperature. \n This assumption is violated in the current
              parameter range")
    }
    Ttor <- ifelse(Ta > Ttormin, Ta, Ttormin) #determinine Ttorpid @ Ta
    Tb <- ifelse(Ttor < Teu, Ttor, Teu) #determine Tb
    values <- c(Ttor = Ttor, WNS = inf,
                growth = FungalGrowthRate(Tb = Tb, fung.params = fung.params)*
                  ScaleFungalGrowthRate(pct.rh = Hd, fung.params = fung.params),
                Eeu = CalcEnergyTimeEuthermic(Ta = Ta, bat.params = bat.params),
                Etor = CalcEnergyTimeTorpid(Ta = Ta, bat.params = bat.params),
                Ear = CalcEnergyArousal(Ttor = Ttor, bat.params = bat.params),
                mod.params)
    ts <- data.table(lsoda(y = c(pT = 1,
                                 pE = 0,
                                 EnergyConsumed = 0,
                                 FungalArea = 0),
                           times = twinter,
                           func = DetModel,
                           parms = values))
    MaxToCurrent <- function(x){
      cummax(x)[-1]
    }
    Ewinter <- MaxToCurrent(ts$EnergyConsumed)
    FatConsumed <- ConvertToFat(Ewinter)
    results <- data.table(Ta = Ta,
                          Humidity = Hd,
                          cbind(GfatConsumed = c(0,FatConsumed),
                                Pdgrowth = c(0,MaxToCurrent(ts$FungalArea)),
                                time = ts$time))
    return(results)
  })
}
