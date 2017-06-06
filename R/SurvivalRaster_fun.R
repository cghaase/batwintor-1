SurvivalRaster{mod.df, hum.rast, temp.rast}{
  require(raster)
  temp.c <- temp.rast - 273
  out <- raster(hum.rast); values(out) <- NA
  out.s <- stack(out,out); names(out.s) <- c("inf.max", "null.max")
  for(j in 1:2){
    ifelse(j == 1, sel <- "inf.max", sel <- "null.max")
    for(i in 1:ncell(hum.rast)){
      out.s[j][i] <- min(mod.df[which(abs(mod.df$humidity-hum.rast[i]) ==
                                 min(abs(mod.df$humidity-hum.rast[i]))
                          & abs(mod.df$Ta-temp.c[i]) ==
                            min(abs(mod.df$Ta-temp.c[i]))), sel])
    }
    out.s[j] <- out.s[j]/30*24
  }
  return(out.s)
}
