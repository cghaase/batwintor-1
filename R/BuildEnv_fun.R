#' Build an environmental space to run the model across
#'
#' \code{BuildEnv} builds the environmental parameters across which the
#' mechanistic model will be applied. This does not necessairly need to
#' represent real-world condtions but can instead be used to explore all of
#' parameter space.
#'
#' @param temp.range a list of two numbers representing the minimum and maximum
#' temperatures to run the model across.
#' @param hum.range a list of two numebers representing the minimum and maximum
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
BuildEnv <- function(temp.range, hum.range, range.res){
  Ta <- seq(from = min(temp.range), to = max(temp.range), by = range.res)
  Hd <- seq(from = min(hum.range), to = max(hum.range), by = range.res)
  env <- expand.grid(Ta,Hd);names(env) <- c("Ta", "Hd")
  return(env)
}
