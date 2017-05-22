## DynamicEnergyPd
data("bat.params"); data("fung.params")
bat.param <- BatLoad(bat.params, "M.californicus")
fung.param <- FungSelect("Verant")
#Create winter parameters to explore
Ta <- seq(-5,25,.5) #ambient temperatures
Hd <- seq(60,100,1) #relative humidity
env <- expand.grid(Ta, Hd); names(env) <- c("Ta", "Hd")
twinter <- seq(0, 9*24*30, 24)
mod.ls <- apply(env, 1, DynamicEnergyPd, inf = T, bat.params = bat.param,
                fung.params = fung.param)
