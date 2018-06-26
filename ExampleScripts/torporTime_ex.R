##CalcTorporTIme Example
data("mylu.params")
fung <- fungalSelect("Chaturvedi")
e <- -10:34 #temperature vector
h <- rep(95, 45) #humidity vector
a <- seq(from = .001, to = 3, length.out = 45) #area vector

r <- torporTime(Ta = e, pct.rh = h, areaPd = 0, WNS = F, bat.params = mylu.params, fung.params = fung)
r1 <- torporTime(Ta = rep(4,45), pct.rh = 95,  areaPd = a, WNS = TRUE, bat.params = mylu.params, fung.params = fung)

plot(e, r, xlab = "Temp (C)", ylab = "Time in Torpor",
     main = "torporTime")
plot(a, r1, xlab = "Area Infected (cm2^)", ylab = "Time in Torpor",
     main = "torporTime")
