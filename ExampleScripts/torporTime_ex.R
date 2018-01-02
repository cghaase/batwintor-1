##CalcTorporTIme Example
data("mylu.params")

e <- -10:34 #temperature vector
a <- seq(from = .001, to = 3, length.out = 45) #area vector

torporTime(Ta = 4, areaPd = 0, inf = TRUE, bat.params = mylu.params)

r  <- torporTime(Ta = e, areaPd = rep(0,45), inf = FALSE, bat.params = mylu.params)
r1 <- torporTime(Ta = rep(4,45), areaPd = a, inf = TRUE, bat.params = mylu.params)

plot(e, r, xlab = "Temp (C)", ylab = "Time in Torpor",
     main = "torporTIme")
plot(a, r1, xlab = "Area Infected (cm2^)", ylab = "Time in Torpor",
     main = "torporTIme")
