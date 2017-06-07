surv.ploter <- function(surv.raster, dist.map, key=NA){
  ###Function for plotting the survival raster @ contiental N. America scale
  ##Arguments:
  ## surv.rast <- Output from survivalRas
  ## dist.map <- Shapfile distrubution of species selected
  require(PBSmapping);require(ggplot2)

  spec.df <- surv.df(surv.raster,dist.map)

  xlim = c(-170,-52)
  ylim = c(10,70)
  worldmap = map_data("world")
  setnames(worldmap, c("X","Y","PID","POS","region","subregion"))
  worldmap = clipPolys(worldmap, xlim=xlim,ylim=ylim, keepExtra=TRUE)
  g.spec <- ggplot(spec.df) +
    aes(x=Longitude, y=Latitude, fill=Months) +
    geom_raster( aes(fill=Months), interpolate = TRUE) +
    geom_polygon(data=worldmap, aes(X,Y,group=PID),
                 alpha=0,
                 fill = "darkseagreen",
                 color="grey50") +
    scale_fill_gradient2 (#low="moccasin",high="green4",
      limits=c(-8,8))
  #geom_text(data = NULL, x = -170, y = 75, label = key))

  return(g.spec)
}
