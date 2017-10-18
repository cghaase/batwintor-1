#' Creates a dataframe from the survival raster for more perfect figure
#' generation.
#'
#' \code{SurvDF} make a pretty dataframe from the output from
#' \code{\link[pkg:batwintor]{SurvivalRaster}}
#'
#' @param surv.raster output from \code{\link[pkg:batwintor]{SurvivalRaster}}
#' @param dist.map shapefile containing the distribution of the species in
#' question.
#' @param nights a raster layer reprensenting the length of winter measured in nigths
#' @return dataframe to be used internally for several of the plotting
#' functions
#' @note will be replaced in the next instance of the model so don't worry that
#' it seems to be completely redundant. it is kinda
#' @export
SurvDF <-function(surv.raster, dist.map = NULL, nights){
  ###Function for converting the survival raster into a dataframe of months survived.
  ##Arguments:
  ## surv.rast <- output from survivalRas()
  ## dist.map <- shapfile distrubution of species selected

  if(extent(surv.raster) == extent(nights)){
    rex <- surv.raster - nights
    tt.spec <- calc(rex, day.to.month)
  } else{
    spec.r.d <- raster::shift(surv.raster,x=-360) # shift the axis for overlay
    spec.r.crop <- crop(spec.r.d,extent(dist.map),weight=T) #crop
    spec.r.mask <- mask(spec.r.crop, dist.map) #mask
    spec.r.months <- calc(spec.r.mask, day.to.month) #covert to months
    coldnightUS <- raster::shift(nights,x=-360) #shift nights
    cold.crop <- crop(coldnightUS,extent(dist.map),weight=T) #crop nights
    spec.night.crop <- mask(cold.crop, dist.map)#mask nights
    spec.months <- calc(spec.night.crop, prop.to.months)
    tt.spec <- spec.r.months - spec.month
  }
  spec.Pt = rasterToPoints(tt.spec)
  spec.df = data.frame(spec.Pt)
  colnames(spec.df) <-c ("Longitude","Latitude","Months")
  return(spec.df)
}
