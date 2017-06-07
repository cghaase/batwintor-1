diff.hist <- function(Comp1.rast, Comp2PD.rast, dist.map, SpeciesName, key=NA, keylocX=-4, keylocY, lsize=1){
  ###Function for creating overlapping histograms of survival length
  ##Arguments:
  ## Comp1.rast <- Output from survivalRas (generally no PD)
  ## Comp2PD.rast <- Output from survivalRas (generally with PD)
  ## dist.map <- Shapfile distrubution of species selected
  ## SpeciesName <- character string for plotting
  ## key <- figure key for plotting
  ## Other key items included
  ##Comp1 df Gen
  Comp1.df <- surv.df(Comp1.rast, dist.map)
  ##Comp2 df Gen
  Comp2.df <- surv.df(Comp2PD.rast, dist.map)
  #Creating Histograms
  dif.df<-data.frame(Months=c(Comp1.df$Months,Comp2.df$Months),
                     WNS=factor(c(rep(paste0(SpeciesName,"Pre"),nrow(Comp1.df)),
                                  rep(paste0(SpeciesName,"Post"),nrow(Comp2.df)))))
  dif.hist.df <- ddply(dif.df, "WNS", summarise, Months.median=median(Months))
  dif.Hist<-ggplot(dif.df, aes(x=Months, fill=WNS)) +
    geom_histogram(binwidth=.5, alpha=.5, position="identity") +xlim(-5,6) +
    geom_vline(data=dif.hist.df, aes(xintercept=Months.median,  colour=WNS),
               linetype="dashed", size=lsize)+
    geom_vline(xintercept = 0)
  dif.Hist<-dif.Hist + geom_text(data = NULL, x = keylocX, y = keylocY, label = key)
  return(dif.Hist)
}
