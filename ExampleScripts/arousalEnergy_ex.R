##CalcEnergyArousal Examples
data("mylu.params")

e <- -10:34 #temperature vector

r <- arousalEnergy(Ta = e, bat.params = mylu.params)

plot(e, r, xlab = "Temp (C)", ylab = "Energy Expended",
     main = "arousalEnergy")
