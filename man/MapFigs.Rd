% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MapFigs_fun.R
\name{MapFigs}
\alias{MapFigs}
\title{MapFigs}
\usage{
MapFigs(surv.stk, dist.map, nights, species.name, save.name)
}
\arguments{
\item{surv.stk}{raster result stack from \code{\link{SurvivalRaster}}}

\item{dist.map}{shapefile distribution of the species being modeled}

\item{nights}{a raster layer reprensenting the length of winter measured in nigths}

\item{species.name}{name of the species for plotting (generally a 4 letter}

\item{save.name}{optional argument. When a path is passed files will save to that
location with provided name. If left as defult 'NULL' will print to plot window}
}
\description{
\code{MapFigs} Creates a strip figure containing survival plots for infected
 and uninfected species and a difference histogram
}
\details{
Strip figures are a combination of \code{\link{SurvPlotter}} and
\code{\link{DiffHist}} all tied together to create one neat strip conviently displaying
the resutlts for a single species in a single function
}
\seealso{
\code{\link{SurvivalRaster}}, \code{\link{SurvPlotter}}, \code{\link{DiffHist}}
}
