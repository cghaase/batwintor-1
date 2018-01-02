##FungalGrowthRate
data("fung.params")
fung.param <- FungSelect("Chaturvedi")

e <- -10:20

r <- fungalGrowth(e, fung.params = fung.param)

plot(e, r, xlab = "Temp (C)", ylab = "Growth rate",
     main = "Fungal Growth Rate")
