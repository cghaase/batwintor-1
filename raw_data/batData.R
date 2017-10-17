"bat.params"
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
  WR <- c(48, 90, 78, 90, 77.4, 77.4)
  CR <- rep(60, 6)
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
