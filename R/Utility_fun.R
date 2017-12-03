# utility functions

# Not in operator
'%!in%' <- function(x,y)!('%in%'(x,y))

# Defaults for NULL values
'%||%' <- function(a, b) if (is.null(a)) b else a

#' @import raster ggplot2 methods
#' @importFrom deSolve lsoda
#' @importFrom data.table data.table rbindlist fread setnames
#' @importFrom dplyr %>% mutate_ filter_ summarise_ ungroup group_by_ quo
#' @importFrom mixtools spEMsymloc
#' @importFrom graphics par mtext title
#' @importFrom grDevices colorRampPalette dev.print dev.off png
#' @importFrom stats median
#' @importFrom rgdal readOGR
#' @importFrom utils data read.csv
#' @importFrom plyr ddply
#' @importFrom PBSmapping clipPolys
#' @importFrom rlang .data
NULL
