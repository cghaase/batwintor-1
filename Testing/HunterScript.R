######################################
##### The Great Bug Hunt of 2017 #####
######################################

mylu.params
fung.ch <- FungSelect("Chaturvedi")
env <- BuildEnv(c(-5,20), c(70,100), 1)
twinter <- seq(from = 0, to = 9*24*30*1, by = 24)


mylu.test <- DynamicEnergyPd(env, mylu.params, fung.ch)
res <- mylu.test


library(dplyr)
ar <- mylu.test %>% filter(time == 6480) %>% select(Prop.Ar)
ta <- unique(mylu.test$Ta)
hum <- unique(mylu.test$humidity)
ar <- matrix(mylu.test$Prop.Ar, length(ta), length(hum))

zar.inf <- res %>%
  group_by(Tb, humidity) %>%
  summarise(max.inf = hour.to.day(max(time*surv.inf))) %>%
  ungroup %>% data.table()

library(fields);library(akima)
?interp
zar.int <- interp(zar.inf$Tb,
                  zar.inf$humidity,
                  (zar.inf$max.inf), duplicate = T)
surface(zar.int,col =color(100),
        ylab = "", xlab = "",
        xlim = c(-1,20), ylim = c(80, 100))

tlab = expression(paste("Temperature (",degree,"C)"))
mtext("% humidity",side=2,outer=T,cex=1.5)
mtext(tlab,side=1,outer=T,cex=1.5)

######
zar.null <- res %>%
  group_by(Ta, humidity) %>%
  summarise(max.null = hour.to.day(max(time*surv.null))) %>%
  ungroup %>% data.table()

library(fields);library(akima)
?interp
zar.int <- interp(zar.null$Ta,
                  zar.null$humidity,
                  (zar.null$max.null), duplicate = T)
surface(zar.int,col =color(100),
        ylab = "", xlab = "",
        xlim = c(-5,20), ylim = c(80, 100))

tlab = expression(paste("Temperature (",degree,"C)"))
mtext("% humidity",side=2,outer=T,cex=1.5)
mtext(tlab,side=1,outer=T,cex=1.5)



t <- seq(-10:20)
e <- CalcEnergyTimeTorpid(t, mylu.params)
plot(t,e)


fat.null <- res %>%
  group_by(Ta, humidity) %>%
  select(n.g.fat.consumed) %>%
  ungroup %>% data.table()

fat.int <- interp(fat.null$Ta,
                  fat.null$humidity,
                  (fat.null$n.g.fat.consumed), duplicate = T)
surface(fat.int,col =color(100),
        ylab = "", xlab = "",
        xlim = c(-5,20), ylim = c(80, 100))

tlab = expression(paste("Temperature (",degree,"C)"))
mtext("% humidity",side=2,outer=T,cex=1.5)
mtext(tlab,side=1,outer=T,cex=1.5)


df.f <- res %>%
  group_by(Ta, humidity) %>%
  select(time, surv.null)%>%
  mutate(max.null = hour.to.day(max(time*surv.null)))%>%
  ungroup %>% data.table()

df.ft <- interp(df.f$time,
                  df.f$Ta,
                  (df.f$max.null), duplicate = T)
surface(df.ft,col =color(100)) #,
        # ylab = "", xlab = "",
        # xlim = c(-5,20), ylim = c(80, 100))
plot(df.f)

df.Gft <- res %>%
  group_by(Ta, humidity) %>%
  select(time, n.g.fat.consumed) %>%
  ungroup %>% data.table()

df.Gft <- interp(df.Gft$time,
                 df.Gft$Ta,
                 (df.Gft$n.g.fat.consumed), duplicate = T)
surface(df.Gft, col= color(100))


df.z <- res %>%
  group_by(Ta, humidity) %>%
  select(time, g.fat.consumed) %>%
  ungroup %>% data.table()

df.zi <- interp(df.z$time,
                df.z$Ta,
                (df.z$g.fat.consumed), duplicate = T)
surface(df.zi, col= color(100),
        ylab = "Ta", xlab = "Time")

df.hi <- interp(df.z$time,
                df.z$humidity,
                (df.z$g.fat.consumed), duplicate = T)
surface(df.hi, col = color(100),
        ylab = "hum", xlab = "Time")
