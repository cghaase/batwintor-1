% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SurvivalRaster_fun.R
\name{SurvivalRaster}
\alias{SurvivalRaster}
\title{Converts model results dataframe into results raster.}
\usage{
SurvivalRaster(mod.df, hum.rast, temp.rast)
}
\arguments{
\item{mod.df}{results from \code{DynamicEnegryPd}}

\item{hum.rast}{raster of humidity}

\item{temp.rast}{raster of temperature values either in Kevil or degrees
Celcius}
}
\description{
\code{SurvivalRaster} uses supplied environmental rasters to query model
results and creating a raster filled with maximum hibernation estimates for
hibernation without infection, with infection and the difference between the
two.
}
\details{
It is suggested that both raster layers have the same extent,
resolution, and projection as there are currently no internal checks on the
consistance between rasters. Values for output are drawn from the humidity
layer.
}
\note{
humidity rasters in the original Hayman et al. 2016 represent the average
of the average humidities for the months Jan-Mar.
}
