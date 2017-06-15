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
DynamicEnergyPd <- function(env.df, bat.params, fung.params){
  require(deSolve); require(data.table); library(dplyr)
  out <- list()
  mod.params <- as.list(c(bat.params, fung.params))
    with(mod.params,{
      for(i in 1:2){ #mechanism for doing both infection status in 1 go
        ifelse(i == 1, inf <- T, inf <- F)
        results <- apply(env.df, 1,function(x){

          Ta <- x[[1]]
          Hd <- x[[2]]
          if(beta3 >= Teu){
            warning("The model assumes fungal growth does not occure at euthermic
                    temperature. \n This assumption is violated in the current
                    parameter range")
          }
          Ttor <- ifelse(Ta > Ttormin, Ta, Ttormin) #determinine Ttorpid @ Ta
          Tb <- ifelse(Ttor < Teu, Ttor, Teu) #determine Tb
          values <- c(Tb = Tb, Ttor = Ttor, WNS = inf,
                      growth = FungalGrowthRate(Tb = Tb, fung.params = mod.params)*
                        ScaleFungalGrowthRate(pct.rh = Hd, fung.params = mod.params),
                      Eeu = CalcEnergyTimeEuthermic(Ta = Ta, bat.params = mod.params),
                      Etor = CalcEnergyTimeTorpid(Ta = Ta, bat.params = mod.params),
                      Ear = CalcEnergyArousal(Ta = Ttor, bat.params = mod.params),
                      Ec = CalcEnergyCool(Ta = Ttor, bat.params = mod.params),
                      mod.params)
          det.results <- data.table(lsoda(y = c(pT = 1,
                                                pE = 0,
                                                pAr = 0,
                                                pC = 0,
                                                EnergyConsumed = 0,
                                                prec.E.arr = 0,
                                                FungalArea = 0),
                                          times = twinter,
                                          func = DetModel,
                                          parms = values))
          MaxToCurrent <- function(x){
            cummax(x)[-1]
          }
          e.winter <- MaxToCurrent(det.results$EnergyConsumed)
          fat.consumed <- ConvertToFat(e.winter)
          results <- data.table(Ta = rep(Ta,length(twinter)),
                                humidity = rep(Hd,length(twinter)),
                                cbind(g.fat.consumed = c(0,fat.consumed),
                                      Pd.growth = c(0,
                                                    MaxToCurrent(det.results$FungalArea)),
                                      time = det.results$time))
          return(results)
        })
      foo <- rbindlist(results)
      out[[i]] <- foo
      }

    out.dt <- cbind(out[[1]], n.g.fat.consumed = out[[2]]$g.fat.consumed)
    out.fin <- out.dt %>%
      mutate(surv.inf = ifelse(mass*.3 >= g.fat.consumed,1,0)) %>%
      mutate(surv.null = ifelse(mass*.3 >= n.g.fat.consumed,1,0))
      return(data.table(out.fin))
    })

}

