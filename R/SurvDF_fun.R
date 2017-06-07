surv.df <-function(surv.raster, dist.map){
  ###Function for converting the survival raster into a dataframe of months survived.
  ##Arguments:
  ## surv.rast <- output from survivalRas()
  ## dist.map <- shapfile distrubution of species selected
  spec.r.d <- raster::shift(surv.raster,x=-360) # shift the axis for overlay
  spec.r.crop <- crop(spec.r.d,extent(dist.map),weight=T)
  spec.r.crop <- mask(spec.r.crop, dist.map)
  coldnightUS <- raster::shift(nights,x=-360)
  cold.crop <- crop(coldnightUS,extent(dist.map),weight=T)
  spec.night.crop <- mask(cold.crop, dist.map)
  tt.spec <- spec.r.crop-spec.night.crop*(365/30)
  spec.Pt = rasterToPoints(tt.spec)
  spec.df = data.frame(spec.Pt)
  colnames(spec.df) <-c ("Longitude","Latitude","Months")
  return(spec.df)
}
