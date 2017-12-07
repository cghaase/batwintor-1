#' batwinto: A pacakge for computing the metabolic costs of hibernation on bats
#' with additional tools to address the impact of White-nose Syndrome on North
#' American species.
#'
#' The batwintor package includes 4 groups of functions, \code{Arrousal
#' Functions}, \code{Torpor Functions}, \code{Model Functions}, and \code{Plot
#' Functions}. In addition there are 3 data sets built into the package that
#' allow you to explore the majorit of the packages functions.
#' @section Data:
#'   There are 3 data sets included within the package.
#' \code{\link{bat.params}}, \code{\link{mylu.params}}, and
#' \code{\link{fung.params}}. \code{bat.params} contains all of the parameter
#' estimations necessary to run the primary model for 12 species of North
#' American bats. Bat species involved are listed by their four letter code
#' (first two letters genus and species) as we got tired of writing out the
#' whole name every time. The parameters for \emph{Myotis
#' lucifugus}(\code{mylu}) is extracted from \code{bat.params} as an example
#' and stored as \code{\link{mylu.params}}. \code{\link{fung,params}} contains
#' the parameters required to calculate and scale the growth of
#' \emph{Pseduogymnoascans destructans}, the psycrophilic fungal agent
#' responsable for WNS.
#' @section Torpor Functions:
#' The \code{Torpor Fucntions} are used to calculate
#' how long a bat will maintain a torpid metabolic state
#' (\code{\link{CalcTorporTimePD}}), which is dependent largley on
#' \code{\link{CalcEWL}}, and \code{\link{CalcEnergyTimeTorpid}} is used to
#' determine the energy expended during those bouts of torpor.
#' @section Arousal Functions
#' The \code{Arousal Functions} are designed to
#' explane the energry expendatures between bouts of torpor.
#' \code{\link{CalcArousalTime}} and \code{\link{CalcEnergyArousal}} document
#' the return from torpid body temperatures ot euthermia
#'
#' @docType package
#' @name batwintor
NULL

#' @import raster ggplot2 methods
#' @importFrom deSolve lsoda
#' @importFrom data.table data.table rbindlist fread setnames
#' @importFrom dplyr %>% mutate filter_ summarise_ ungroup group_by_ quo
#' @importFrom mixtools spEMsymloc
#' @importFrom graphics par mtext title
#' @importFrom grDevices colorRampPalette dev.print dev.off png
#' @importFrom stats median
#' @importFrom rgdal readOGR
#' @importFrom utils data read.csv
#' @importFrom plyr ddply
#' @importFrom PBSmapping clipPolys
#' @importFrom rlang .data

