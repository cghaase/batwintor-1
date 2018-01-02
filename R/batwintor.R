#' batwintor: A pacakge for computing the metabolic costs of hibernation on bats
#' with additional tools to address the impact of White-nose Syndrome on North
#' American species.
#'
#' The batwintor package includes 4 groups of functions, \code{Arrousal
#' Functions}, \code{Torpor Functions}, \code{Model Functions}, and \code{Plot
#' Functions}. In addition there are 3 data sets built into the package that
#' allow you to explore the majorit of the packages functions.
#' @section Data:
#' There are 3 data sets included within the package.
#' \code{\link{bat.params}}, \code{\link{mylu.params}}, and
#' \code{\link{fung.params}}. \code{bat.params} contains all of the parameter
#' estimations necessary to run the primary model for 12 species of North
#' American bats. Bat species involved are listed by their four letter code
#' (first two letters genus and species) as we got tired of writing out the
#' whole name every time. The parameters for \emph{Myotis
#' lucifugus}(\code{mylu}) is extracted from \code{bat.params} as an example
#' and stored as \code{\link{mylu.params}}. \code{\link{fung.params}} contains
#' the parameters required to calculate and scale the growth of
#' \emph{Pseduogymnoascans destructans}, the psycrophilic fungal agent
#' responsable for WNS.
#'
#' @section Torpor Functions:
#' The \code{Torpor Fucntions} are used to calculate
#' how long a bat will maintain a torpid metabolic state
#' (\code{\link{torporTime}}), which is dependent largley on
#' \code{\link{ewl}}, and \code{\link{torporEnergy}} is used to
#' determine the energy expended during those bouts of torpor.
#'
#' @section Arousal Functions:
#' The \code{Arousal Functions} are designed to
#' explane the energry expendatures between bouts of torpor.
#' \code{\link{arousalTime}} and \code{\link{CalcEnergyArousal}} document
#' the return from torpid body temperatures to euthermia. The functions
#' \code{\link{euthermicEnergy}} describes energy expendature during
#' euthermia, and the \code{\link{flyingTime}} and
#' \code{\link{flyingEnergy}} functions describe ammendments made to encourperate
#' the metabolic costs of flight during arousals. The \code{\link{CalcCoolTime}}
#' and \code{\link{coolEnergy}} functions handle the return from eutheria
#' to torpid state and complete the arousal cycle.
#'
#' @section Model Functions:
#' The model functions are the heart of the package and are composed of:
#' \code{\link{buildEnv}}, \code{\link{hibernationModel}}, and
#' \code{\link{DetModel}}. The \code{buildEnv} function allows you to create your
#' the parameter space that your model will run across in the form of
#' environmental spave and a time vector. The \code{hibernationModel} takes
#' the supplied environment, the parameters passed in and formats the data
#' for the \code{DetMod} which solves the differential equations.
#' \code{hibernationModel} then formats, and outputs the results for post-processing
#' and plotting
#'
#' @section Plot Tools:
#'
#'
#' @docType package
#' @name batwintor
NULL
