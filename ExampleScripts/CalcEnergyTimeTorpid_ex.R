##CalcEnergyTimeTorpid Example
data("mylu.params")

e <- -10:34 #temperature vector

r <- CalcEnergyTimeTorpid(e, mylu.params)

plot(e, r, xlab = "Temp (C)", ylab = "Energy Expended",
     main = "CalcEnergyTimeTorpid")
