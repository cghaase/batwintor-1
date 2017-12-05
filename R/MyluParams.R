#' Metabolic paramerters for \emph{Myotis lucifigus}.
#'
#' A dataset containing information on euthermic, and torpid metabolic rates,
#' laong with other parameters used throughout this
#' package for \emph{Myotis lucifigus} (little brown bat).
#'
#' @format a dataset containg 24 measurements for 1 species
#' \describe{
#'   \item{RMR}{Resting metabolic rate in volume O2 mL/h/g}
#'   \item{TMRmin}{minimum metabolic rate during torpor in volume O2 mL/h/g}
#'   \item{Teu}{euthermic temperature in degrees C}
#'   \item{Tlc}{lower critical temperature in degrees C}
#'   \item{Ttormin}{Temperature at which TMRmin is achieved in degrees C}
#'   \item{Ceu}{conductance during euthermic temperatures mLO2/g/C}
#'   \item{Ct}{conductance during torpor mLO2/g/C}
#'   \item{S}{Constant, specific heat of tissue mLO2/g/C}
#'   \item{ttormax}{maximal length of time for a bout of torpor in hours}
#'   \item{teu}{time spent euthermic during a bout of torpor in hours}
#'   \item{mass}{animal mass in grams}
#'   \item{WR}{warming rate from torpor to euthermic temperature in degrees C/
#'     hour}
#'   \item{CR}{cooling rate from euthermic temperature to torpor in degrees C/
#'     hour}
#'   \item{rEWL}{rate of evaporative water loss (mg/h/dWVP/cm2)}
#'   \item{mrPd}{metabolic rate increase due to infection}
#'   \item{aPd}{increase of total evaporative water loss due to fungal growth}
#'   \item{rPd}{area specific rate of evaporative water loss due to infection}
#'   \item{pMass}{precent of body mass selected to trigger evaporative water
#'   loss arrousals}
#'   \item{pMass.i}{precent of body mass selected to trigger evaporative water
#'   loss arrousals within infected animals}
#'   \item{pFly}{proportion time of euthermic arrousals spent flying}
#'   \item{pFat}{proportion of body mass that is fat}
#'   \item{pLean}{proportion of body mass that is lean mass}
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
#' @references Haymen et al. 2016, Bat Ecology
#'
"mylu.params"
