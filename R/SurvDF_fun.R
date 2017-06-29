#' Creates a dataframe from the survival raster for more perfect figure
#' generation.
#'
#' \code{SurvDF} make a pretty dataframe from the output from
#' \code{\link(SurvivalRaster)}
#'
#' @param surv.raster output from \code{\link{SurvivalRaster}}
#' @param dist.map shapefile containing the distribution of the species in
#' question.
#' @return dataframe to be used internally for several of the plotting
#' functions
#' @note will be replaced in the next instance of the model so don't worry that
#' it seems to be completely redundant. it is kinda
#'
SurvDF <-function(surv.raster, dist.map, nights){
  ###Function for converting the survival raster into a dataframe of months survived.
  ##Arguments:
  ## surv.rast <- output from survivalRas()
  ## dist.map <- shapfile distrubution of species selected
  spec.r.d <- raster::shift(surv.raster,x=-360) # shift the axis for overlay
  spec.r.crop <- crop(spec.r.d,extent(dist.map),weight=T)
  spec.r.mask <- mask(spec.r.crop, dist.map)
  spec.r.months <- calc(spec.r.mask, function(x){x/(30*24)})
  coldnightUS <- raster::shift(nights,x=-360)
  cold.crop <- crop(coldnightUS,extent(dist.map),weight=T)
  spec.night.crop <- mask(cold.crop, dist.map)
  spec.months <- calc(spec.night.crop, function(x){x*(365/30)})
  tt.spec <- spec.r.months - spec.months
  spec.Pt = rasterToPoints(tt.spec)
  spec.df = data.frame(spec.Pt)
  colnames(spec.df) <-c ("Longitude","Latitude","Months")
  return(spec.df)
}
