##CalcEnergyTimeTorpid Example
data("mylu.params")

e <- -10:34 #temperature vector
a <- seq(from = .001, to = 3, length.out = 45) #area vector

r <- torporEnergy(Ta = e, WNS = F, areaPd = 0, bat.params = mylu.params)
r1 <- torporEnergy(Ta = rep(4,45), areaPd = a, WNS = T, bat.params = mylu.params)
plot(e, r, xlab = "Temp (C)", ylab = "Energy Expended",
     main = "torporEnergy")
plot(a, r1, xlab = "Area Infected (cm2^)", ylab = "Energy Expended",
     main = "torporEnergy")
