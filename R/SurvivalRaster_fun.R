#' Converts model results dataframe into results raster.
#'
#' \code{SurvivalRaster} uses supplied environmental rasters to query model
#' results and creating a raster filled with maximum hibernation estimates.
#'
#' @param mod.df results from \code{\link{DynamicEnegryPd}}
#' @param hum.rast raster of humidity values for extent
#' @param temp.rast raster of temperature values
#'
#' @details This function will be replaces with a direct raster method in the
#' next build of this package.
#'
#' humidity rasters in the original Hayman et al. 2016 represent the average
#' of the average humidities for the months Jan-Mar.
#'
#' @note This function as is is pretty much shite and takes for.ev.er. use at
#' your own risk, and really try to do it only once.
#'
#' @return A raster of the same extent as \code{hum.rast} filled with the
#' maximal hibernation estimates from \code{\link{DynamicEnegryPd}}.
#'
#' @family PlotTools
#'
#' @example please see vignette for usage
SurvivalRaster <- function(mod.df, hum.rast, temp.rast){
  require(raster);require(dplyr)
  temp.c <- temp.rast - 273
  out <- raster(hum.rast); values(out) <- NA
  out.s <- list(out,out); names(out.s) <- c("max.inf", "max.null")
  mod.dif <- mod.df %>%
    dplyr::group_by(Ta, humidity) %>%
    dplyr::summarise(max.null = max(time*surv.null),max.inf = max(time*surv.inf)) %>%
    dplyr::mutate(diff = (max.inf - max.null)/(24*30)) %>%
    ungroup %>% data.table
  out.l <- c()
  for(j in 1:2){
    ifelse(j == 1, sel <- "max.inf", sel <- "max.null")
    for(i in 1:ncell(hum.rast)){
      #values(out.s[j][i])
      out.l[[i]] <- min(mod.dif[which(abs(mod.dif$humidity-hum.rast[i]) ==
                                 min(abs(mod.dif$humidity-hum.rast[i]))
                          & abs(mod.dif$Ta-temp.c[i]) ==
                            min(abs(mod.dif$Ta-temp.c[i]))),
                          sel, with = FALSE])
    }
    values(out.s[[j]]) <- out.l
  }
  return(out.s)
}
