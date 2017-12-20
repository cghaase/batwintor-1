#' Build an environmental space to run the model across
#'
#' \code{buildEnv} builds the environmental parameters across which the
#' dynamic model will be applied. This does not necessairly need to
#' represent real-world condtions but can instead be used to explore all of
#' parameter space.
#'
#' @param temp a \code{\link[raster]{raster}} or a list of two numbers representing the
#'  minimum and maximum temperatures to run the model across.
#' @param pct.rh a \code{\link[raster]{raster}} or a list of two numebers representing
#'  the minimum and maximum percent relative humidity to run the model across.
#' @param range.res.temp a single numeber representing the resultion of
#' \code{temp.range}.
#' @param range.res.rh a single numeber representing the resultion of \code{pct.rh.range}.
#' @param twinter the maximal length of winter to run the model across in either days or
#' months.
#' @param winter.res temporal resolution of the \code{twinter} vector. Default is
#' hourly, although other options can be selected.
#'
#' @return returns an expanded dataframe containing all possiable combinations of
#' temperature and humidity conditions across which the model will be run, as well
#' as vector to act as the temporal vector to run the model across
#'
#' @details This step can be a major determinate of how fast the model will run
#' . If the \code{temp.range} and \code{pct.rh.range} and large or
#' \code{range.res} is especially fine, this may present computational
#' challenges downstream.
#' @family Model Engine
#' @seealso \code{\link{hibernationModel}}
#' @example ExampleScripts/buildEnv_ex.R
#' @export
buildEnv <- function(temp, pct.rh, range.res.temp = 1, range.res.rh = 1,  twinter, winter.res = 24){
  #Raster methods
  if(methods::is(temp, "Raster") || methods::is(pct.rh, "Raster")){
    t <- setMinMax(temp)
    #Check units (only useful for discovering if units are K or C)
    ifelse( minValue(t) > 150,
            temp.range <- c(minValue(t), maxValue(t))-273,#from K to C
            temp.range <- c(minValue(t), maxValue(t)))
    h <- setMinMax(pct.rh)
    pct.rh.range <- c(round(minValue(h)), maxValue(h))
  } else{
    temp.range <- temp
    pct.rh.range <- pct.rh
  }

  Ta <- seq(from = min(temp.range), to = max(temp.range), by = range.res.temp)
  pct.rh <- seq(from = min(pct.rh.range), to = max(pct.rh.range), by = range.res.rh)
  env <- expand.grid(Ta,pct.rh);names(env) <- c("Ta", "pct.rh")
  ifelse(twinter > 12,
         twin <- seq(from=0, to = day.to.hour(twinter), by = winter.res),
         twin <- seq(from=0, month.to.hour(twinter), by = winter.res))
  cat("The ENV proposed will consist of ", nrow(env)*length(twin) ," calculations.
      If this is too many please considering changing the vector resolution")

  out <- list(env = env, twinter = twin)
  class(out) <- c("WinterEnv")
  return(out)
}
