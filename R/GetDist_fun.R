#' Obtatin the species distribution for a species.
#'
#' \code{GetDist} obtain the IUCN distribution for a mammaian species.
#'
#' @param path.to.data a path to the IUCN MAMMTERR data source.
#' @param species binomial latin name (or list of names) as a character
#' string.
#'
#' @return Returns a named shapefile(s) of the speceies input to the global
#' environment
#' @details The IUCN MAMMTERR data set includes distribution shapefiles for
#' most terrestial mammals however they are not without their flaws so use
#' accordingly. Data can be sourced from
#' \link{\url{http://www.iucnredlist.org/technical-documents/spatial-data}}
#'
#' @note This may take a while as the IUCN database is large. May be a point
#' to fix in the future.
#'
#' #@example TODO? is it needed
#' @export
GetDist <- function(path.to.data, species){
  full <- readOGR(dsn = file.path(file.path(path.to.data,"/MAMMTERR")),
                  layer = "Mammals_Terrestrial")
  out <- list()
  for(i in 1:length(species)){
    if(species[i] %in% full$BINOMIAL){
      out[[i]] <- full[full$BINOMIAL %in% species[i],]
    }else{
      cat(paste0(species[i])," was not found in the data set. Was there a
          spelling error by chance?")
    }
  }
}
