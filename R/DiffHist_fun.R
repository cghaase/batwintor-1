#' Creats histograms displaying the model resulst from the species distribution
#'
#' \code{DiffHist} Two hisograms are created and overlayed over one another.
#' The histograms represent the survival capasitites obtained through
#' \code{\link{DynamicEnergyPd}} across the distribution. one will contain the
#' results of null hibernation, and the other infection with WNS.
#' @return returns two histograms overlaying one another with median estimations
#'
#' @param surv.stk raster result stack from \code{\link{SurvivalRaster}}
#' @param dist.map shapefile distribution of the species being modeled
#' @param species.name name of the species for plotting (generally a 4 letter
#' abreviation will work best)
#' @param nights a raster layer reprensenting the length of winter measured in nigths
#' @param save.name optional argument. When a path is passed files will save to that
#' location with provided name. If left as defult 'NULL' will print to plot window
#' @param ... more things that are generally usefully but yeah.
#'
#' @details This function will likely go the way of the dodo with the next model
#' updates and build because this is unnecessairy convaluted.
#' @family PlotFunctions
#' @seealso \code{\link{DangerZone}}; \code{\link{MapFigs}}; \code{\link{SurvPlotter}}
#' @export
DiffHist <- function(surv.stk, dist.map, species.name, nights, save.name=NULL, ...){
  Comp1.df <- SurvDF(surv.stk$"max.null", dist.map, nights = nights)
  Comp2.df <- SurvDF(surv.stk$"max.inf", dist.map, nights = nights)
  dif.df<-data.frame(Months=c(Comp1.df$Months,Comp2.df$Months),
                     WNS=factor(c(rep(paste0(species.name,"Pre"),nrow(Comp1.df)),
                                  rep(paste0(species.name,"Post"),nrow(Comp2.df)))))
  dif.hist.df <- plyr::ddply(dif.df, "WNS", summarise, Months.median=median(Months))
  dif.Hist<-ggplot(dif.df, aes(x=Months, fill=WNS)) +
    geom_histogram(binwidth=.5, alpha=.5, position="identity") +xlim(-5,6) +
    geom_vline(data=dif.hist.df, aes(xintercept=Months.median,  colour=WNS),
               linetype="dashed", size=lsize)+
    geom_vline(xintercept = 0)
  dif.Hist<-dif.Hist

  if(!is.null(save.name)){
    dev.print(width = 600, height = 600, png, paste0(save.name))
    dev.off()
  }
  return(dif.Hist)
}
