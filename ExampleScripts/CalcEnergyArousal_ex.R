##CalcEnergyArousal Examples
data("bat.params")

myyu.params <- BatLoad(bat.params, species = "M.californicus")

CalcEnergyArousal(Ttor = 4, bat.params = myyu.params)
