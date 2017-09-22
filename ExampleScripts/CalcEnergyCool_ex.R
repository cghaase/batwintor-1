##CalcEnergyCool
data("mylu.params")

e <- -10:34 #temperature vector

r <- CalcEnergyCool(Ta = e, bat.params = mylu.params)

plot(e, r, xlab = "Temp (C)", ylab = "Energy Expended",
     main = "CalcEnergyCool")
