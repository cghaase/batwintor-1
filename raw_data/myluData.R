"mylu.params"
if("mylu.params.rda" %!in% list.files("data/")){
  p.names <- c("RMR", "TMRmin", "Teu", "Tlc", "Ttormin", "Ceu", "Ct", "S",
               "ttormax", "teu", "mass", "WR", "CR", "rEWL", "wing.area",
               "colony.size", "SA.body", "SA.wing", "pmass", "mrPd", "aPd",
               "rPd")
  mylu <- list(2.6, 0.03, 35, 30.3, 2, 0.2638, 0.055, 0.171, 792, 3, 7, 48, 52,
               0.147569, 88, 3000, 36.59, 124.59, 0.043, 1.4, 0.21, 1.525)
  mylu.params <- mylu ; names(mylu.params) <- p.names
  devtools::use_data(mylu.params, overwrite = T)
}
