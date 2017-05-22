#' Select from built in funal growth parameters.
#'
#' \code{FungSelect} select between presets of scaled fungal growth to apply
#' to the model.
#'
#' @param choose choose one of either "Chaturvedi" or "Verant"
#' @details This is your last chance. After this, there is no turning back. You
#'  take the blue pill—the story ends, you wake up in your bed and believe what
#'  ever you want to believe. You take the red pill—you stay in Wonderland, and
#'   I show you how deep the rabbit hole goes. Remember: all I'm offering is
#'   the truth. Nothing more.
#'
#' Growth parameters are either: "Chaturvedi" (faster growth) from
#' chatruvedi et al. PLoS One, ot "Verant" (slower growth rate), from Verent et
#' al. PLoS One.
#'
#' \strong{mu1}: scaling parameter for Michaelis-Menton function
#'
#' \strong{mu2}: scaling parameter for Michaelis-Menton function
#'
#' \strong{beta1}: temperature dependant hourly rate shape parameter
#'
#' \strong{beta2}: temperature dependant hourly rate shape parameter
#'
#' \strong{beta3}: temperature dependant hourly rate shape parameter
#'
#' @return Returns a named list of fungal growth scaling parameters
#' @seealso \code{\link{FungLoad}}
FungSelect <- function(choose){
  data("fung.params")
  as.list(fung.params[choose,])
}
