#' Dynamic energy cost calculation
#'
#' \code{hibernationModel} determines the growth area of Pd and the sums the
#' energy consumed under specified environmental conditions.
#'
#' @param env an object produced by \code{\link{buildEnv}} conataing range of env conditions
#' and a time vector to run the model across
#' @param bat.params  parameters returned by \code{bat.params}
#' @param fung.params parameters returned by \code{fung.params}
#'
#' @return returns a dataframe containing a model results for each set of environmental
#' conditions at each time point.
#'
#' @details This function formats, data going into, and processes data coming out of
#' \code{\link{DetModel}}, and is the main main function of the package.
#' Outputs from ths function can be exceedingly large, and take a heckin' long time
#' so please plan accordingly.
#'
#' @family Model Engine
#' @seealso \code{\link{DetModel}}, \code{\link{buildEnv}}
#' @example ExampleScripts/hibernationModel_ex.R
#' @export
hibernationModel <- function(env, bat.params, fung.params){
  out <- list()
  mod.params <- as.list(c(bat.params, fung.params))
    with(mod.params,{
      #Mechanism to do WNS + and - in one function
      for(i in 1:2){
        ifelse(i == 1, inf <- T, inf <- F)
        # apply model engine across env dataframe
        results <- apply(env[[1]], 1,function(x){
          Ta <- x[[1]]
          pct.rh <- x[[2]]
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
          values <- c(Tb = Tb, Ttor = Ttor, WNS = inf, pct.rh= pct.rh,
                      # Fungal growth area
                      growth = fungalGrowth(Tb = Tb, fung.params = mod.params)*
                        ScalefungalGrowth(pct.rh = pct.rh, fung.params = mod.params),
                      # Energy cost for euthermia
                      Eeu = euthermicEnergy(Ta = Ta, bat.params = mod.params),
                      # Energy costs for flying during euthermia
                      Efl = flyingEnergy(Ta = Ta, bat.params = mod.params),
                      # Energy cost for torpor
                      Etor = torporEnergy(Ta = Ta, bat.params = mod.params),
                      # Energy cost for arousal from torpor
                      Ear = CalcEnergyArousal(Ta = Ttor, bat.params = mod.params),
                      # Energy cost for cooling from euthermic
                      Ec = coolEnergy(Ta = Ttor, bat.params = mod.params),
                        mod.params)
          # Call differential equation model
          det.results <- data.table(lsoda(y = c(pT = 1, # Inital values
                                                pAr = 0,
                                                pC = 0,
                                                pE = 0,
                                                pFl = 0,
                                                EnergyConsumed = 0,
                                                EnergyBoutArrousal = 0,
                                                FungalArea = 0),
                                          # Time to solve across
                                          times = env[[2]],
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
          ar.winter <- MaxToCurrent(det.results$EnergyBoutArrousal)
          # Convert units to grams of fat
          fat.consumed <- kcal.to.g(e.winter)
          ar.fat <- kcal.to.g(ar.winter)
          # What precent of costs are due to arousals
          prec.ar <- ar.fat/fat.consumed
          # Proportion of time in torpor
          prop.tor <- MaxToCurrent(det.results$pT)
          prop.ar <- MaxToCurrent(det.results$pAr)
          prop.fl <- MaxToCurrent(det.results$pFl)
          Tb <- Tb
          # Creat dataframe of results for intermediate product
          results <- data.table(Ta = rep(Ta,length(env[2])),
                                pct.rh = rep(pct.rh,length(env[2])),
                                cbind(g.fat.consumed = c(0,fat.consumed),
                                      pEnergyBoutArrousal = c(0, prec.ar),
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
                    n.pEnergyBoutArrousal = out[[2]]$pEnergyBoutArrousal,
                    n.Prop.tor = out[[2]]$Prop.tor,
                    n.Prop.Ar = out[[2]]$Prop.Ar,
                    n.Prop.Fl = out[[2]]$Prop.Fl)
    # Create columns with survival outcomes  based on avaliable fat reserves
    out.fin <- out.dt %>%
      mutate(surv.inf = ifelse(mass*pFat >= g.fat.consumed,1,0)) %>%
      mutate(surv.null = ifelse(mass*pFat >= n.g.fat.consumed,1,0))
      return(data.table(out.fin))
    })
}

