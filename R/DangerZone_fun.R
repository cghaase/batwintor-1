danger.zone <-function(mod.df, speciesOption="", save.name=NULL){
  require(dplyr);require(data.table);require(fields);require(akima)
   mod.dif <- mod.df %>%
     dplyr::group_by(Ta, humidity) %>%
     dplyr::summarise(max.null = max(time*surv.null),max.inf = max(time*surv.inf)) %>%
     dplyr::mutate(diff = (max.inf - max.null)/(24*30)) %>%
    ungroup %>% data.table

  zzg <- interp(mod.dif$Ta, # T # can use interp.loess or interp function instead for regular spaced data
                  mod.dif$humidity, # H
                  (mod.dif$diff), # time in months
                  duplicate=T)
  color<-colorRampPalette(c( "red","yellow","white","blue"))
  par(omi=c(1,1,0.5,1))
  par(mai=c(0.8,0.8,0.8,0.8))
  par(mfrow=c(1,1))
  par(cex.lab=1)
  par(cex.axis=1)
  par(cex.sub=1)
  par(cex.main=1)
  par(cex=1)
  surface(zzg,col =color(100),
          ylab = "", xlab = "",
          xlim = c(-1,20), ylim = c(80, 100))

  tlab = expression(paste("Temperature (",degree,"C)"))
  mtext("% humidity",side=2,outer=T,cex=1.5)
  mtext(tlab,side=1,outer=T,cex=1.5)
  mtext("Decreased survival time (Months)",side=4,outer=T,line=1,cex=1.5)
  key <- paste0("Hibernacula danger zone for ",speciesOption)
  title(key, cex = 2)
  par(adj=0)
  }
}
