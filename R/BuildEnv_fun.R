#' Build an environmental space to run the model across
#'
#' \code{BuildEnv} builds the environmental parameters across which the
#' mechanistic model will be applied. This does not necessairly need to
#' represent real-world condtions but can instead be used to explore all of
#' parameter space.
#'
#' @param temp a raster or a list of two numbers representing the minimum and maximum
#' temperatures to run the model across.
#' @param hum a raster or a list of two numebers representing the minimum and maximum
#' percent relative humidity to run the model across.
#' @param range.res a single numeber representing the resultion of
#' \code{temp.range} and \code{hum.range}.
#'
#' @return returns a dataframe containing all possiable combinations of
#' temperature and humidity conditions across which the model will be run.
#'
#' @details This step can be a major determinate of how fast the model will run
#' . If the \code{temp.range} and \code{hum.range} and large or
#' \code{range.res} is especially fine, this may present computational
#' challenges downstream.
#'
#' @example ExampleScripts/BuildEnv_ex.R
BuildEnv <- function(temp, hum, range.res = 1){
  #Raster methods
  if(is(temp, "Raster") || is(hum, "Raster")){
    t <- setMinMax(temp)
    #Check units (only useful for discovering if units are K or C)
    ifelse( minValue(t) > 150,
            temp.range <- c(minValue(t), maxValue(t))-273,#from K to C
            temp.range <- c(minValue(t), maxValue(t)))
    h <- setMinMax(hum)
    hum.range <- c(round(minValue(h)), maxValue(h))
  } else{
    temp.range <- temp
    hum.range <- hum
  }

  Ta <- seq(from = min(temp.range), to = max(temp.range), by = range.res)
  Hd <- seq(from = min(hum.range), to = max(hum.range), by = range.res)
  env <- expand.grid(Ta,Hd);names(env) <- c("Ta", "Hd")
  cat("The ENV proposed will consist of ", nrow(env)*(9*24*30) ," calculations.
      If this is too many please considering changing the vector resolution")
  return(env)
}
