##ScaleFungalGrowthRate
foo <- "paramFiles/"
fung.param <- FungLoad(path.to.params = foo,
                       growth.option = "Verant")
ScaleFungalGrowthRate(pct.rh = 98, fung.params = fung.param)
