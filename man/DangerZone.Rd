% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DangerZone_fun.R
\name{DangerZone}
\alias{DangerZone}
\title{Plotting the difference between infected and and uninfected hibernating
bats.}
\usage{
DangerZone(mod.df, species.option = "", save.name = NULL)
}
\arguments{
\item{mod.df}{results data from \code{\link{DynamicEnergyPd}}}

\item{species.option}{chracter string used for labeling}

\item{save.name}{a name and file path relative to the working directory
where the figure will be saved.}
}
\value{
returns an environmental surface accross which the difference
between infected and uninfected hibernation times is plotted.
}
\description{
\code{DangerZone} plots the difference in hibernation length potentials
between infected and uninfected bats accross the environmental space.
}
\details{
Function will be updated in the future to include more plotting
parameter accessability, and potentially a raster method as well.
\url{https://www.youtube.com/watch?v=kyAn3fSs8_A}
}
\author{
L. Cain, K. Loggins
}
