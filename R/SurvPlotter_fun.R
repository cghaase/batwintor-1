#' Plots survival raster results including a nice outline of North America.
#'
#' \code{SurvPlotter} plots the results from \code{\link{SurvivalRaster}} and
#' adds shapes of North America.
#'
#' @param surv.stk raster result stack from \code{\link{SurvivalRaster}}
#' @param WNS logical. Should bats have WNS
#' @param dist.map shapefile representing the distribution you wish to map the
#' results accross (generally speaking the entire distribution of the species)
#' @param nights a raster layer reprensenting the length of winter measured in nigths
#'
#' @return returns a map of North America in which the distribution supplied
#' is filled in with the results from the model run.
#'
#' @note This function will be removed/ remodeled with the next version
#' @family Plot Functions
#' @seealso \code{\link{DangerZone}}; \code{\link{MapFigs}}; \code{link{survialRaster}};
#' \code{\link{DiffHist}}
#' @export

SurvPlotter <- function(surv.stk, WNS, dist.map, nights){
  ifelse(WNS == T, #WNS Flag
         spec.df <- SurvDF(surv.stk$"max.inf",dist.map,nights),
         spec.df <- SurvDF(surv.stk$"max.null",dist.map,nights))

  worldmap = map_data("world")
  setnames(worldmap, c("X","Y","PID","POS","region","subregion"))
  worldmap = PBSmapping::clipPolys(worldmap,
                       xlim=extent(surv.stk)[1:2],
                       ylim=extent(surv.stk)[3:4],
                       keepExtra=TRUE)
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
    # geom_polygon(data = fortify(dist.map),
    #              aes(long,lat,group=group),
    #              colour = "black", fill = NA) +
    scale_fill_gradient2(low="red4",mid = "white",high="midnightblue"
      #limits=c((min(minValue(stack(surv.rasters))/720)-1)
       #        ,(max(maxValue(stack(surv.rasters))/720)+1))
      ) +
    coord_fixed()
  bkg <- theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

  return(x <- g.spec + bkg)
}
