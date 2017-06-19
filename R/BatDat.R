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
#' @seealso data("mylu.params"), data("fung.params")
#'
#' @references Haymen et al. 2016
"bat.params"
'%!in%' <- function(x,y)!('%in%'(x,y))
if("bat.params.rda" %!in% list.files("data/")){
  bats <- c("M.lucifigus",
            "M.myotis",
            "E.serotinus",
            "E.fuscus",
            "M.yumanensis",
            "M.californicus")
  RMR <- c(2.6,	1, 1.596296296, 1.13, 2.301, 3.425)
  TMRmin <- c(0.03,	0.2, 0.026431718,	0.028, 0.0467, 0.033)
  Teu <- c(35, 35, 35, 35, 33.4, 32.4)
  Tlc <- rep(32, 6)
  Ttormin <- c(2, 10, 6, 3.5, 4, 6)
  Ceu <- rep(.2638, 6)
  Ct <- rep(.055, 6)
  S <- rep(.131, 6)
  ttormax <- c(rep(792,5),408)
  tar <- rep(.75,6)
  teu <- c(rep(3,5),2.1666)
  mass <- c(9, 25, 22.7, 15, 5.55, 5.4)
  WR <- rep(NA, 6)
  CR <- rep(NA, 6)
  bat.params <- cbind(RMR = RMR,
                      TMRmin = TMRmin,
                      Teu = Teu,
                      Tlc = Tlc,
                      Ttormin = Ttormin,
                      Ceu = Ceu,
                      Ct = Ct,
                      S = S,
                      ttormax = ttormax,
                      tar = tar,
                      teu = teu,
                      mass = mass,
                      WR = WR,
                      CR = CR)
  rownames(bat.params) <- bats
  devtools::use_data(bat.params, overwrite = T)
}


