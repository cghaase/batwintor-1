#' Dynamic enegry cost calculation
#'
#' \code{DynamicEnegryPd} determines the growth area of Pd and the sums the
#' enegry consumed under specified environmental conditions.
#'
#' @param env.df dataframe conataing range of env conditions
#' @param inf infection status (\code{TRUE} or \code{FALSE})
#' @param bat.params parameters returned by \code{bat.params}
#' @param fung.params parameters returned by \code{fung.params})
#'
#' @details TODO
#' @seealso \code{\link{DetModel}}
#' @example TODO
#'
DynamicEnegryPd <- function(env.df, inf, bat.params, fung.params){
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
                Eeu <- CalcEnergyTimeEuthermic(Ta = Ta, bat.params = bat.params),
                Etor <- CalcEnegryTimeTorpid(Ta = Ta, bat.params = bat.params),
                Ear <- CalcEnegryArousal(Ttor = Ttor, bat.params = bat.params),
                mod.params)
    ts <- data.table(lsoda(y = c(pT = 1,
                                 pE = 0,
                                 EnegryConsumed = 0,
                                 FungalArea = 0),
                           times = twinter,
                           func = DetModel,
                           parms = values))
    MaxToCurrent <- function(x){
      cummax(x)[-1]
    }
    Ewinter <- MaxToCurrent(ts$EnegryConsumed)
    FatConsumed <- ConvertToFat(Ewinter)
    results <- data.table(Ta = Ta,
                          Humidity = Hd,
                          cbind(GfatConsumed = c(0,FatConsumed),
                                Pdgrowth = c(0,MaxToCurrent(ts$FungalArea)),
                                time = ts$time))
    return(results)
  })
}
