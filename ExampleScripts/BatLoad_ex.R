##BatLoad Example
dat <- read.csv("paramFiles/species.parms.csv")
print(colnames(dat)) # All but "Parameter" are valid for species argument

myyu.params <- BatLoad(dat, species = "M.californicus")
print(str(myyu.params))
