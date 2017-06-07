SurvivalRaster <- function(mod.df, hum.rast, temp.rast){
  require(raster)
  temp.c <- temp.rast - 273
  out <- raster(hum.rast); values(out) <- NA
  out.s <- list(out,out); names(out.s) <- c("max.inf", "max.null")
  mod.dif <- mod.df %>%
    dplyr::group_by(Ta, humidity) %>%
    dplyr::summarise(max.null = max(time*surv.null),max.inf = max(time*surv.inf)) %>%
    dplyr::mutate(diff = (max.inf - max.null)/(24*30)) %>%
    ungroup %>% data.table
  out.l <- c()
  for(j in 1:2){
    ifelse(j == 1, sel <- "max.inf", sel <- "max.null")
    for(i in 1:ncell(hum.rast)){
      #values(out.s[j][i])
      out.l[[i]] <- min(mod.dif[which(abs(mod.dif$humidity-hum.rast[i]) ==
                                 min(abs(mod.dif$humidity-hum.rast[i]))
                          & abs(mod.dif$Ta-temp.c[i]) ==
                            min(abs(mod.dif$Ta-temp.c[i]))), ..sel])
    }
    values(out.s[[j]]) <- out.l
  }
  return(out.s)
}
