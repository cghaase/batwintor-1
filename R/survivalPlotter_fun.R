#' Plots survival raster results including a nice outline of North America.
#'
#' \code{survivalPlotter} plots the results from \code{\link{survivalRaster}} and
#' adds shapes of North America.
#'
#' @param surv.stk raster result stack from \code{\link{survivalRaster}}
#' @param WNS logical. Should the bats plotted be infected with WNS?
#' @param dist.map shapefile representing the distribution you wish to map the
#' results accross (generally speaking the entire distribution of the species)
#' @param nights a raster layer reprensenting the length of winter measured in nigths
#'
#' @return returns a map of North America in which the distribution supplied
#' is filled in with the results from the model run.
#'
#' @family Plot Functions
#' @seealso \code{\link{dangerZone}}; \code{\link{MapFigs}}; \code{link{survialRaster}};
#' \code{\link{survivalHistogram}}
#' @export

survivalPlotter <- function(surv.stk, WNS, dist.map, nights, save.name = NULL, ...){
  if(WNS == T){
    surv <- surv.stk[[1]]
  }else{surv <- surv.stk[[2]]}

  rex <- surv - nights#ghts
  tt.spec <- calc(rex, day.to.month)
  sp.c <- mask(crop(tt.spec, dist.map), dist.map)
  spec.Pt = rasterToPoints(sp.c)
  spec.df = data.frame(spec.Pt)
  colnames(spec.df) <-c ("long","lat","Months")


  g.spec <- ggplot(spec.df) +
    coord_fixed()+
    borders("world",
            xlim=extent(surv)[1:2],ylim=extent(surv)[3:4],
            colour = "grey20",
            fill = "grey80")+
    geom_raster( aes_(~long, ~lat,fill=~Months), interpolate = TRUE)+
    geom_polygon(data = fortify(dist.map),
                 aes_(~long,~lat, group = ~group),
                 colour = "black",
                 fill = NA) +
    scale_fill_gradientn("Survival\nCapacity\n(months)",
                         colors = c("#e66101", "#fdb863","#ffffff", "#b2abd2", "#5e3c99"),
                         limits=  c(-8,8))+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(size = 18,  family="serif"),
          axis.title = element_text(size = 16,  family="serif"),
          axis.text = element_text(size = 16,  family="serif"),
          legend.key.size = unit(42, "points"),
          legend.title = element_text(size = 16,  family="serif"),
          legend.text = element_text(size = 16,  family="serif"),
          panel.background = element_rect(fill = "lightblue", colour = "lightblue",size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))

  if(!is.null(save.name)){
    ggsave( filename = save.name, ...)
    return(invisible(NULL))
  }


  return(g.spec)
}
