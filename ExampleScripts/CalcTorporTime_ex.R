##CalcTorporTIme Example
data("mylu.params")

e <- -10:34 #temperature vector
a <- seq(from = .001, to = 3, length.out = 45) #area vector

CalcTorporTime(Ta = 4, areaPd = 0, inf = T, bat.params = mylu.params)

r  <- CalcTorporTime(Ta = e, areaPd = rep(0,45), inf = F, bat.params = mylu.params)
r1 <- CalcTorporTime(Ta = rep(4,45), areaPd = a, inf = T, bat.params = mylu.params)

plot(e, r, xlab = "Temp (C)", ylab = "Time in Torpor",
     main = "CalcTorporTIme")
plot(a, r1, xlab = "Area Infected (cm2^)", ylab = "Time in Torpor",
     main = "CalcTorporTIme")
