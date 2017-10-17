#' Modeling parameters for 6 species of hibernating bats.
#'
#' A dataset containing information regarding morphometrics, euthermic and torpor metabolic rates of
#' 6 species of hibernating bats.
#'
#' @format a dataset containg 14 measurements for 6 species
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
#'   \item{tar}{time take to arouse from torpor in hours}
#'   \item{teu}{time spent euthermic during a bout of torpor in hours}
#'   \item{mass}{animal mass in grams}
#'   \item{WR}{warming rate from torpor to euthermic temperature in degrees C/
#'     hour}
#'   \item{CR}{cooling rate from euthermic temperature to torpor in degrees C/
#'     hour}
#'   }
#' @details Within this data set, and generally through out the package
#' \code{T} will represent a temperature value, while \code{t} will represent
#' a time value.
#'
#' Naming of variables is largely drawn from the "Physiological
#' Ecology and Energetics of Bats" by Speakman and Thomas contained within
#' Bat Ecology.
#'
#' \emph{M. yumanensis} WR is the same as \emph{M. californicus} because of the
#' similarity in body mass and distribution between the two species.
#'
#' All species were given the cooling rate \code{CR} 60 as a good estimation
#'
#' @seealso data("mylu.params"), data("fung.params")
#'
#' @references Haymen et al. 2016; Menzies et al. 2016
