"fung.params"
if("fung.params.rda" %!in% list.files("data/")){

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
