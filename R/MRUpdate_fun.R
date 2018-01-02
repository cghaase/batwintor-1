#' Updates build-in metabolic parameter estimates using new data.
#'
#' \code{mrUpdate} updates paremeter estimates supplied in
#' \code{\link{bat.params}} with estimates obtained through the use of
#' \code{\link{mrRaw}} for use throughout the package.
#'
#' @param x object returned by \code{\link{mrRaw}}
#' @param species a character vector of the species to be updated. This should
#' be a row name from the \code{bat.params}
#' @param params a dataframe of metabolic parameters (generally that returned
#' with \code{\link{bat.params}})
#'
#' @details Generally speaking not all of the metabolic parameters documented
#' in \code{\link{bat.params}} are updated at once, and therefore
#' only those that have new estimates are updated.
#'
#' @seealso \code{\link{bat.params}}, \code{\link{mrRaw}},
#' \code{\link{batLoad}}
#' @example ExampleScripts/mrUpdate_ex.R
#' @export
mrUpdate <- function(x, species, params = data("bat.params", package = "batwintor")){
  bat.update <- params
  x.df <- x$df
  for(i in 1:length(names(x.df))){
    bat.update[species,names(x.df)[i]] <- x.df[,names(x.df)[i]]
  }
  return(bat.update[species,])
}

