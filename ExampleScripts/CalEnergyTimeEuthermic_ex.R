##CalEnergyTimeEuthermic Example
data("bat.params")

myyu.params <- BatLoad(bat.params, species = "M.californicus")

CalcEnergyTimeEuthermic(4, myyu.params)
