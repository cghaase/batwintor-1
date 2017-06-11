#' Growth coeffecients for funal growth models
#'
#' The beta and mu growth coeffecients for two different models of fungal growth.
#'
#' @format a data set including 3 beta parameters and 2 mu parameters for 2
#'  models
#' \describe{
#'   \item{beta1}{beta1}
#'   \item{beta2}{beta2}
#'   \item{beta3}{beta3}
#'   \item{mu1}{mu1}
#'   \item{mu2}{mu2}
#' }
'%!in%' <- function(x,y)!('%in%'(x,y))
if("fung.params.rda" %!in% list.files("data/")){
  "fung.params"
  beta1 <- c(0.0007751467,	0.0001762951)
  beta2 <- c(0.2699683006,	0.2699682306)
  beta3 <- c(19.7303882961,	19.7303885569)
  mu1 <- c(0.000150958,	0.000150958)
  mu2 <- c(-0.0099245944,	-0.0099245944)
  fung.params <- cbind(beta1 = beta1, beta2 = beta2, beta3 = beta3, mu1 = mu1,
                       mu2 = mu2)
  rownames(fung.params) <- c("Chaturvedi", "Verant")
  devtools::use_data(fung.params, overwrite = T)
}

