##CalcCoolTime
data("mylu.params")

e <- seq(-10:34) #temperature vector

r <- CalcCoolTime(Ta = e, bat.params = mylu.params)

plot(e, r, xlab = "Temp (C)", ylab = "Time")
