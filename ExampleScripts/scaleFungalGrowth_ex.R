##ScaleFungalGrowthRate
data("fung.params")
fung.param <- fungalSelect("Verant")

rh <- 1:100

r <- scaleFungalGrowth(pct.rh = rh, fung.params = fung.param)

plot(rh, r, xlab = "Relative humidity (%)", ylab = "Growth Rate",
     main = "scaledFungalGrowth")
