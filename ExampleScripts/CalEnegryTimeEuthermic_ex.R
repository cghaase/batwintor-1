##CalEnegryTimeEuthermic Example
dat <- read.csv("paramFiles/species.parms.csv")

myyu.params <- BatLoad(dat, species = "M.californicus")

CalcEnergyTimeEuthermic(4, myyu.params)
