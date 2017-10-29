## DynamicEnegryPd_ex
data("bat.params")
data("fung.params")

my.params <- bat.params["M.californicus",]
fung.ch <- FungSelect("Chaturvedi")


env.df <- BuildEnv(temp = c(1:10),
                hum = c(90:100),
                range.res = 1)

twinter <- seq(from = 0, to = 9*24*30*1, by = 24)
bingo <- DynamicEnergyPd(env.df, my.params, fung.ch)
