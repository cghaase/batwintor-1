#' Plots survival raster results including a nice outline of North America.
#'
#' \code{SurvPlotter} plots the results from \code{\link{SurvivalRaster}} and
#' adds shapes of North America.
#'
#' @param surv.raster return product from \code{\link{SurvivalRaster}}
#' @param dist.map shapefile representing the distribution you wish to map the
#' results accross (generally speaking the entire distribution of the species)
#' @param key you can add a key to the figure for easy multi figure assemblage
#'
#' @return returns a map of North America in which the distribution supplied
#' is filled in with the results from the model run.
#'
#' @note This function will be removed/ remodeled with the next version

SurvPlotter <- function(surv.stk, WNS, dist.map,
                        nights, key=NA){
  ###Function for plotting the survival raster @ contiental N. America scale
  ##Arguments:
  ## surv.rast <- Output from survivalRas
  ## dist.map <- Shapfile distrubution of species selected
  require(PBSmapping);require(ggplot2)
  require(maptools); require(plyr)

  ifelse(WNS == T,
         spec.df <- SurvDF(surv.stk[[1]],dist.map,nights),
         spec.df <- SurvDF(surv.stk[[2]],dist.map,nights))

  xlim = c(-170,-52)
  ylim = c(10,70)
  worldmap = map_data("world")
  setnames(worldmap, c("X","Y","PID","POS","region","subregion"))
  worldmap = clipPolys(worldmap, xlim=xlim,ylim=ylim, keepExtra=TRUE)
  g.spec <- ggplot(spec.df) +
    aes(x=Longitude, y=Latitude) +
    geom_polygon(data=worldmap, aes(X,Y,group=PID),
                 alpha= .5,
                 fill = "darkolivegreen2",
                 color="grey20") +
    geom_raster( aes(fill=Months), interpolate = TRUE) +
    geom_polygon(data=worldmap, aes(X,Y,group=PID),
                 alpha= .0001,
                 fill = "snow",
                 color="grey20") +
    #geom_polygon(data = as.data.frame(dist.map), aes(long,lat), colour = "black", fill = NA) +
    geom_polygon(data = fortify(dist.map),
                 aes(long,lat,group=group),
                 colour = "black", fill = NA) +
    scale_fill_gradient2(low="red4",mid = "white",high="midnightblue"
      #limits=c((min(minValue(stack(surv.rasters))/720)-1)
       #        ,(max(maxValue(stack(surv.rasters))/720)+1))
      )
  bkg <- theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )
  #geom_text(data = NULL, x = -170, y = 75, label = key))

  return(g.spec + bkg)
}
