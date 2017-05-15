##CalcEnegryTimeTorpid Example
dat <- read.csv("paramFiles/species.parms.csv")

myyu.params <- BatLoad(dat, species = "M.californicus")

CalcEnegryTimeTorpid(4, myyu.params)
