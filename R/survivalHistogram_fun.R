#' Creats histograms displaying the model resulst from the species distribution
#'
#' \code{survivalHistogram} Two hisograms are created and overlayed over one another.
#' The histograms represent the survival capasitites obtained through
#' \code{\link{hibernationModel}} across the distribution. one will contain the
#' results of null hibernation, and the other infection with WNS.
#' @return returns two histograms overlaying one another with median estimations
#'
#' @param surv.stk raster result stack from \code{\link{survivalRaster}}
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
#' @seealso \code{\link{dangerZone}}; \code{\link{survivalMultiplot}}; \code{\link{survivalPlotter}}
#' @export
survivalHistogram <- function(surv.stk, dist.map, species.name, nights, save.name=NULL, ...){
  inf <- surv.stk[[1]] - nights
  nul <- surv.stk[[2]] - nights
  stk.cal <- stack(inf, nul); names(stk.cal) <- c("inf", "null")
  tt.spec <- calc(stk.cal, day.to.month)
  sp.c <- mask(crop(tt.spec, dist.map), dist.map)
  spec.Pt = rasterToPoints(sp.c)
  Comp1.df = as.data.frame(spec.Pt)
  colnames(Comp1.df) <-c ("long","lat","inf", "null")

  oz <- data.frame(Months = c(Comp1.df$inf, Comp1.df$null),
                   WNS = c(rep("inf", nrow(Comp1.df)),
                           rep("null", nrow(Comp1.df))))
  med.inf <- oz %>%
    group_by(WNS) %>%
    summarise(med = median(Months))
  dif.Hist<-ggplot(oz, aes_(x=~Months, fill = ~WNS, color=~WNS)) +
    scale_color_manual(values=c("#5e3c99", "#e66101")) +
    scale_fill_manual(values=c("#5e3c99", "#e66101")) +
    geom_histogram(binwidth=.5, alpha=.5, position="identity") +xlim(-8,8) +
    geom_vline(data=med.inf, aes_(xintercept=~med,  colour= ~WNS),
               linetype="dashed", size = 1)+
    geom_vline(xintercept = 0) +
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(size = 18,  family="serif"),
          axis.title = element_text(size = 16,  family="serif"),
          axis.text = element_text(size = 16,  family="serif"),
          legend.key.size = unit(42, "points"),
          legend.title = element_text(size = 16,  family="serif"),
          legend.text = element_text(size = 16,  family="serif"),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())


  if(!is.null(save.name)){
    ggsave( filename = save.name, ...)
    return(invisible(NULL))
  }
  return(dif.Hist)
}
