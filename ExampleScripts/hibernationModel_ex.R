## DynamicEnegryPd_ex
data("mylu.params")
data("fung.params")

fung.ch <- fungalSelect("Chaturvedi")


env.df <- buildEnv(temp = c(1:10),
                pct.rh = c(90:100),
                range.res.temp = 1,
                range.res.rh = 1,
                twinter = 9,
                winter.res = 4)

bingo <- hibernationModel(env.df, mylu.params, fung.ch)
