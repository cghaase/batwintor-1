##CalcEnergyArousal Examples
dat <- read.csv("paramFiles/species.parms.csv")

myyu.params <- BatLoad(dat, species = "M.californicus")

CalcEnergyArousal(Ttor = 4, bat.params = myyu.params)
