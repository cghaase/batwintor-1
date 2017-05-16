#' Calculate enegry use and fungal area as a function of environmental
#' condtions and infection status
#'
#' \code{EnegryPd} calculates enegry consumed and the area of funal growth
#' given static environmental conditions across a predefined length of winter.
#'
#' @param Ta ambient temperature
#' @param twinter length of winter
#' @param Hd humidity
#' @param bat.prams parameters returned by \code{\link{LoadBat}}
#' @param fung.params parameters returned by \code{\link{LoadFung}}
#'
#' @details TODO
#' Does this function need to exist?
#' @examples TODO
EnegryPd <- function(Ta, twinter, Hd, bat.params, fung.params){
  mod.params <- as.list(c(bat.params, fung.params))
  with(mod.params,{
    Ttor <- ifelse
  })
}
