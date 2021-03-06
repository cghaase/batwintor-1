#' Metabolic paramerters for \emph{Myotis lucifigus}.
#'
#' A dataset containing information on euthermic, torpid metabolic rates,
#' surface area estimations, and other parameters used throughout this
#' package for \emph{Myotis lucifigus} (little brown bat).
#'
#' @format a dataset containg 24 measurements for 1 species
#' \describe{
#'   \item{RMR}{Resting metabolic rate in volume O2 mL/h/g}
#'   \item{TMRmin}{minimum metabolic rate during torpor in volume O2 mL/h/g}
#'   \item{Teu}{euthermic temperature in degrees C}
#'   \item{Tlc}{lower critical temperature in degrees C}
#'   \item{Ttormin}{Temperature at which TMRmin is achieved in degrees C}
#'   \item{Ceu}{conductance during euthermic temperatures (TODO units)}
#'   \item{Ct}{conductance during torpor (TODO units)}
#'   \item{S}{specific heat of tissue (TODO units)}
#'   \item{ttormax}{maximal length of time for a bout of torpor in hours}
#'   \item{teu}{time spent euthermic during a bout of torpor in hours}
#'   \item{mass}{animal mass in grams}
#'   \item{WR}{warming rate from torpor to euthermic temperature in degrees C/
#'     hour}
#'   \item{CR}{cooling rate from euthermic temperature to torpor in degrees C/
#'     hour}
#'   \item{rEWL}{rate of evaporative water loss}
#'   \item{wing.area}{No clue what that one is.}
#'   \item{colony.size}{estimate of the number of individuals in a hibernacula}
#'   \item{SA.body}{estimate of the body surface area in cm^2}
#'   \item{SA.wing}{estimate of the wing surface area in cm^2}
#'   \item{pmass}{precent of body mass selected to trigger evaporative water
#'   loss arrousals}
#'   \item{mrPd}{No clue what this one is. TODO}
#'   \item{aPd}{No clue what this one is. TODO}
#'   \item{rPd}{No clue what this one is. TODO}
#'
#'   }
#' @details Within this data set, and generally through out the package
#' \code{T} will represent a temperature value, while \code{t} will represent
#' a time value.
#'
#' Naming of variables is largely drawn from the "Physiological
#' Ecology and Energetics of Bats" by Speakman and Thomas contained within
#' Bat Ecology.
#'
#' @seealso data("bat.params"), data("fung.params")
#'
#' @references Haymen et al. 2016
#'
"mylu.params"
