#' Plotting the difference between infected and and uninfected hibernating
#' bats.
#'
#' \code{DangerZone} plots the difference in hibernation length potentials
#' between infected and uninfected bats accross the environmental space.
#'
#' @return returns an environmental surface accross which the difference
#' between infected and uninfected hibernation times is plotted.
#'
#' @param mod.df results data from \code{\link{DynamicEnergyPd}}
#' @param species.option chracter string used for labeling
#' @param save.name a name and file path relative to the working directory
#' where the figure will be saved.
#'
#' @details Function will be updated in the future to include more plotting
#' parameter accessability, and potentially a raster method as well.
#' \url{https://www.youtube.com/watch?v=kyAn3fSs8_A}
#' @author L. Cain, K. Loggins
#' @family Plot Functions
#' @seealso \code{\link{MapFigs}}; \code{\link{SurvPlotter}}; \code{\link{DiffHist}}
#' @export
DangerZone <-function(mod.df, species.option="", save.name=NULL){
   mod.dif <- mod.df %>%
     group_by_(~Ta, ~pct.rh) %>%
     summarise_(max.null = ~max(time*surv.null),max.inf = ~max(time*surv.inf)) %>%
     mutate_(diff = ~(max.inf - max.null)/(24*30)) %>%
     ungroup %>% data.table

  zzg <- akima::interp(mod.dif$Ta, # T # can use interp.loess or interp function instead for regular spaced data
                  mod.dif$pct.rh, # H
                  (mod.dif$diff), # time in months
                  duplicate=T)
  color<-colorRampPalette(c( "red","yellow","green4"))
  par(omi=c(1,1,0.5,1))
  par(mai=c(0.8,0.8,0.8,0.8))
  par(mfrow=c(1,1))
  par(cex.lab=1)
  par(cex.axis=1)
  par(cex.sub=1)
  par(cex.main=1)
  par(cex=1)
  fields::surface(zzg,col =color(100),
          ylab = "", xlab = "",
          xlim = c(-1,20), ylim = c(80, 100))

  tlab = expression(paste("Temperature (",degree,"C)"))
  mtext("% humidity",side=2,outer=T,cex=1.5)
  mtext(tlab,side=1,outer=T,cex=1.5)
  mtext("Decreased survival time (Months)",side=4,outer=T,line=1,cex=1.5)
  key <- paste0("Hibernacula danger zone for ",species.option)
  title(key, cex = 2)
  par(adj=0)

  if(!is.null(save.name)){
    dev.print(width = 600, height = 600, device = png, paste0(save.name, ".png"))
    dev.off()
  }
  return(invisible(NULL))
}
