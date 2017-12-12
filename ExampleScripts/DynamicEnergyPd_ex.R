## DynamicEnegryPd_ex
data("bat.params")
data("fung.params")

my.params <- bat.params["myca",]
fung.ch <- FungSelect("Chaturvedi")


env.df <- BuildEnv(temp = c(1:10),
                pct.rh = c(90:100),
                range.res.temp = 1,
                range.res.rh = 1,
                twinter = 9,
                winter.res = 4)

bingo <- DynamicEnergyPd(env.df, my.params, fung.ch)
