##ScaleFungalGrowthRate
data("fung.params")
fung.param <- FungSelect("Verant")

rh <- 1:100

r <- ScaleFungalGrowthRate(pct.rh = rh, fung.params = fung.param)

plot(rh, r, xlab = "Relative humidity (%)", ylab = "Growth Rate",
     main = "ScaledFungalGrowthRate")
