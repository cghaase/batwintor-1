##CalcArousalTime
data("mylu.params")

e <- -10:34 #temperature vector

r <- CalcArousalTime(Ta = e, bat.params = mylu.params)

plot(e, r, xlab = "Temp (C)", ylab = "Time to Arrousal",
     main = "CalcArousalTime")
