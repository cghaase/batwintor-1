#' Dynamic energy cost calculation
#'
#' \code{DynamicEnergyPd} determines the growth area of Pd and the sums the
#' energy consumed under specified environmental conditions.
#'
#' @param env.df dataframe conataing range of env conditions
#' @param bat.params  parameters returned by \code{\link[pkg:batwintor]{BatLoad}}
#' @param fung.params parameters returned by \code{\link[pkg:batwintor]{FungLoad}})
#'
#' @details TODO
#' @seealso \code{\link[pkg:batwintor]{DetModel}}
#' @example ExampleScripts/DynamicEnergyPd_ex.R
#' @export
DynamicEnergyPd <- function(env.df, bat.params, fung.params){
  out <- list()
  mod.params <- as.list(c(bat.params, fung.params))
    with(mod.params,{
      #Mechanism to do WNS + and - in one function
      for(i in 1:2){
        ifelse(i == 1, inf <- T, inf <- F)
        # apply model engine across env dataframe
        results <- apply(env.df, 1,function(x){
          Ta <- x[[1]]
          Hd <- x[[2]]
          if(beta3 >= Teu){
            warning("The model assumes fungal growth does not occure at euthermic
                    temperature. \n This assumption is violated in the current
                    parameter range")
          }
          # determinine Ttorpid @ Ta
          Ttor <- ifelse(Ta > Ttormin, Ta, Ttormin)
          # determine Tb
          Tb <- ifelse(Ttor < Teu, Ttor, Teu)
          # create values that will be fed into the dynamic model
          values <- c(Tb = Tb, Ttor = Ttor, WNS = inf, Hd= Hd,
                      # Fungal growth area
                      growth = FungalGrowthRate(Tb = Tb, fung.params = mod.params)*
                        ScaleFungalGrowthRate(pct.rh = Hd, fung.params = mod.params),
                      # Energy cost for euthermia
                      Eeu = CalcEnergyTimeEuthermic(Ta = Ta, bat.params = mod.params),
                      # Energy costs for flying during euthermia
                      Efl = CalcEnergyFlying(Ta = Ta, bat.params = mod.params),
                      # Energy cost for torpor
                      Etor = CalcEnergyTimeTorpid(Ta = Ta, bat.params = mod.params),
                      # Energy cost for arousal from torpor
                      Ear = CalcEnergyArousal(Ta = Ttor, bat.params = mod.params),
                      # Energy cost for cooling from euthermic
                      Ec = CalcEnergyCool(Ta = Ttor, bat.params = mod.params),
                        mod.params)
          # Call differential equation model
          det.results <- data.table(lsoda(y = c(pT = 1, # Inital values
                                                pE = 0,
                                                pFl = 0,
                                                pAr = 0,
                                                pC = 0,
                                                EnergyConsumed = 0,
                                                prec.E.arr = 0,
                                                FungalArea = 0),
                                          # Time to solve across
                                          times = twinter,
                                          func = DetModel,
                                          parms = values))
          # Helper function for energy calculations
          # This is needed because the dif eqs solve for the change in (x)
          # per time, not (x) @ t
          MaxToCurrent <- function(x){
            cummax(x)[-1]
          }
          # Energy costs for up to that point in the winter
          e.winter <- MaxToCurrent(det.results$EnergyConsumed)
          # Convert units to grams of fat
          fat.consumed <- ConvertToFat(e.winter)
          # Energy cost for arousals up to that point in the winter
          ar.winter <- MaxToCurrent(det.results$prec.E.arr)
          # Convert arousal costs to grams fat
          ar.fat <- ConvertToFat(ar.winter)
          # What precent of costs are due to arousals
          prec.ar <- ar.fat/fat.consumed
          # Proportion of time in torpor
          prop.tor <- MaxToCurrent(det.results$pT)
          prop.ar <- MaxToCurrent(det.results$pAr)
          prop.fl <- MaxToCurrent(det.results$pFl)
          Tb <- Tb
          # Creat dataframe of results for intermediate product
          results <- data.table(Ta = rep(Ta,length(twinter)),
                                humidity = rep(Hd,length(twinter)),
                                cbind(g.fat.consumed = c(0,fat.consumed),
                                      prec.ar = c(0, prec.ar),
                                      Pd.growth = c(0,
                                                    MaxToCurrent(det.results$FungalArea)),
                                      time = det.results$time,
                                      Prop.tor = c(1,prop.tor),
                                      Prop.Ar = c(0,prop.ar),
                                      Prop.Fl = c(0,prop.fl),
                                      Tb = Tb))
          return(results)
        })
      foo <- rbindlist(results)
      out[[i]] <- foo
      }
 # Create one better dataframe with all pertinent columns
    out.dt <- cbind(out[[1]], n.g.fat.consumed = out[[2]]$g.fat.consumed,
                    n.prec.ar = out[[2]]$prec.ar)
    # Create columns with survival outcomes  based on avaliable fat reserves
    out.fin <- out.dt %>%
      mutate(surv.inf = ifelse(mass*.3 >= g.fat.consumed,1,0)) %>%
      mutate(surv.null = ifelse(mass*.3 >= n.g.fat.consumed,1,0))
      return(data.table(out.fin))
    })
}

