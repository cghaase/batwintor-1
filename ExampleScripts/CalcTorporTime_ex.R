##CalcTorporTIme Example
dat <- read.csv("paramFiles/species.parms.csv")

myyu.params <- BatLoad(dat, species = "M.californicus")

CalcTorporTime(Ta = 4, areaPd = 0, inf = F, bat.params = myyu.params)

CalcTorporTime(Ta = 4, areaPd = 4, inf = T, bat.params = myyu.params)
