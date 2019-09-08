library(batwintor)
library(ggplot2)
library(beepr)
# library(data.table)
# library(deSolve)
# library(dplyr)

#Load fungal growth
fung.params <- fungalSelect("Chaturvedi")

rh2wvp <- function(pct.rh, Ta){
  wvp = (pct.rh*0.01)*(0.61 * exp((17.50 * Ta)/(Ta + 240.97)))
  return(wvp)
}

dwvp <- function(pct.rh, Ta){
  wvp = (pct.rh*0.01)*(0.61 * exp((17.50 * Ta)/(Ta + 240.97)))
  dwvp = (0.61 * exp((17.50 * Ta)/(Ta + 240.97))) - wvp
  return(dwvp)
}

################################################################################
#### Figure out increase to TMR due to Pd growth                            ####
################################################################################
#These data are from Liam's paper, figure 1
#days in torpor were 113
tmr = c(3.2, 3.80137) #pulled from wet
max = fungalGrowth(Tb = 7, fung.params = fung.params, t.min = 0)*scaleFungalGrowth(pct.rh = 98, fung.params = fung.params)*113*24
df <- data.frame(Treatment = c("Begin", "End"), Growth = c(0,max/19.9*100), TMR = tmr)
summary(lm(df$TMR~df$Growth))

################################################################################
#### Determine thermal conductance during torpor                            ####
################################################################################

#Read in TMR data
TMR <- read.csv("C:/Users/Katie Haase/Documents/TMR.csv")

#Remove 0 data
TMR <- TMR[TMR$Mass.specific.TMR_mlO2>0,]

#Create function
fun <- function(data){lm(data$Mass.specific.TMR_mlO2~data$Actual.Temperature)}

#Apply to each species
fun(TMR[TMR$Bat.Species == "Corynorhinus townsendii",])
fun(TMR[TMR$Bat.Species == "Myotis velifer",])
fun(TMR[TMR$Bat.Species == "Myotis lucifugus",])
fun(TMR[TMR$Bat.Species == "Perimyotis subflavus",])
fun(TMR[TMR$Bat.Species == "Eptesicus fuscus",])

################################################################################
#### Determine proportion of euthermia spent flying                         ####
################################################################################
#Should match 88 & 114 days [Warnecke et al 2012 PNAS]
mylu.params <- BatLoad(bat.params, "mylu")

#Build environment
env.df  <- BuildEnv(temp = c(7,7), pct.rh = c(97,97), range.res.temp = 1, range.res.rh = 1, twinter = 10, winter.res = 1)

#Allow prop of flying to vary
s <- seq(0.05, 1, 0.05)
df <- data.frame()
for(f in s){
  mylu.params$pFly = f
  mylu.mod <- DynamicEnergyPd(env = env.df,  bat.params = mylu.params, fung.params = fung.params)
  mylu.mod.null = mylu.mod[mylu.mod$surv.null == 1]
  mylu.mod.null = mylu.mod.null[mylu.mod.null$time == max(mylu.mod.null$time)]
  mylu.mod.inf = mylu.mod[mylu.mod$surv.inf == 1]
  mylu.mod.inf = mylu.mod.inf[mylu.mod.inf$time == max(mylu.mod.inf$time)]
  all.df <- data.frame(Ta = mylu.mod.null$Ta, pct.rh = mylu.mod.null$pct.rh, pFly = mylu.params$pFly, n.g.fat.consumed = mylu.mod.null$n.g.fat.consumed, NullTime = mylu.mod.null$time,
                       g.fat.consumed = mylu.mod.inf$g.fat.consumed, InfectTime = mylu.mod.inf$time)
  df <- rbind(df, all.df)
}

#Plot survival over prop flying
plot(df$pFly, df$InfectTime/24, col = "white", xlab = "Proportion Spent Flying", ylab = "Predicted Survival (days)", cex.axis = 1.5, cex.lab = 1.5)
lines(df$pFly, df$InfectTime/24, lwd = 2)
lines(df$pFly, rep(114, length(df$pFly)), lwd = 2, lty = 2, col = "red")

################################################################################
#### Fit species-specific lines to EWL & body mass                          ####
################################################################################
#Read in EWL/TMR data
TMR <- read.csv("C:/Users/Katie Haase/Documents/TMR.csv")

#Cut to dry measurements data only
TMR <- TMR[is.na(TMR$EWL) == FALSE & TMR$Treatment == "dry",]

#Calculate mean per individual
EWL.mean <- data.frame(EWL.mean = tapply(TMR$EWL, INDEX = as.factor(as.character(TMR$Bat.ID)), FUN = mean, na.rm=TRUE))

#Recreate data.frame
df.ewl <- data.frame(BatID = rownames(EWL.mean), Species = TMR$Bat.Species[match(rownames(EWL.mean), TMR$Bat.ID)], EWL.mean = EWL.mean, Mass = TMR$Mass..prior.resp.[match(rownames(EWL.mean), TMR$Bat.ID)])

#Fit linear model to all species
plot(log(df.ewl$Mass), log(df.ewl$EWL.mean/df.ewl$Mass))
summary(lm(log(EWL.mean/Mass) ~ log(Mass), data = df.ewl))

#Fit linear model per species
df.mod <- data.frame()
for(s in unique(TMR$Bat.Species)){
  lm.data <- df.ewl[df.ewl$Species == s,]
  plot(lm.data$Mass, lm.data$EWL.mean/lm.data$Mass)
  mod <- lm(log(EWL.mean/Mass) ~ log(Mass), data = lm.data)
  print(summary(mod))
  df.mod <- rbind(df.mod, data.frame(s,t(data.frame(coef(mod)))))
  rownames(df.mod) = c()
}
colnames(df.mod) = c("Species","Intercept","log(Mass)")


################################################################################
#### Sensitivity analysis                                                   ####
################################################################################
library(lhs)

#Assign bat parameters
mylu.params <- batLoad(bat.params, "MYLU")

#Determine number of bins/intervals for PRCC
nspaces=100

#Create a LHS function with the N of columns that matches parameters
hypercube=randomLHS(n=nspaces, k=26)

#Create range of parameters (k has to equal number listed)
mins = with(c(as.list(mylu.params),as.list(fung.params)),
            c(   			    ## set mins for each parameters-exclude those not to be varied if any
              Mass = 0.9*7.8,
              RMR = 0.9*2.6,
              TMRmin = 0.9*0.14,
              Teu = 0.9*37,
              Tlc = 0.9*34,
              Ttormin = 0.9*2,
              Ceu = 0.9*0.2638,
              Ct = 0.9*0.2,
              ttormax = 0.9*792,
              teu = 0.9*3,
              WR = 0.9*48,
              mrPd = 0.9*.01539,
              aPd = 0.9*0.16,
              pMass = 0.9*0.027,
              pMass.i = 0.9*0.027,
              pLean = 0.9*0.532,
              pFat = 0.9*0.216,
              SA.wing = 0.9*19.6,
              SA.plagio = 0.9*8.66,
              rEWL.body = 0.9*0.1,
              rEWL.wing = 0.9*0.3278,
              beta1 = 0.9*0.0007751467,
              beta2 = 0.9*0.2699683,
              beta3 = 0.9*19.7309,
              mu1 = 0.9*0.000150958,
              mu2 = 0.9*-0.009924594
        ))

maxs = with(c(as.list(mylu.params),as.list(fung.params)),
            c( 				    ## set maxs for each parameters-exclude those not to be varied if any
              Mass = 1.1*7.8,
              RMR = 1.1*2.6,
              TMRmin = 1.1*0.14,
              Teu = 1.1*37,
              Tlc = 1.1*34,
              Ttormin = 1.1*2,
              Ceu = 1.1*0.2638,
              Ct = 1.1*0.2,
              ttormax = 1.1*792,
              teu = 1.1*3,
              WR = 1.1*48,
              mrPd = 1.1*.01539,
              aPd = 1.1*0.16,
              pMass = 1.1*0.027,
              pMass.i = 1.1*0.027,
              pLean = 1.1*0.532,
              pFat = 1.1*0.216,
              SA.wing = 1.1*19.6,
              SA.plagio = 1.1*8.66,
              rEWL.body = 1.1*0.1,
              rEWL.wing = 1.1*0.3278,
              beta1 = 1.1*0.0007751467,
              beta2 = 1.1*0.2699683,
              beta3 = 1.1*19.7309,
              mu1 = 1.1*0.000150958,
              mu2 = 1.1*-0.009924594
      ))

diffs=maxs-mins ## range of each variable

#Create copy of hypercube samples to modify, hypercube adjusted; i.e. new matrix
hypercubeadj = hypercube
for (i in 1:ncol(hypercube)){
  hypercubeadj[,i]=hypercube[,i]*diffs[i]+mins[i] # scale samples to difference and add minimum
}

#Give names of parameters -- makes sure the same order as above
dimnames(hypercubeadj)[[2]]=c(
                              "Mass",
                              "RMR",
                              "TMRmin",
                              "Teu",
                              "Tlc",
                              "Ttormin",
                              "Ceu",
                              "Ct",
                              "ttormax",
                              "teu",
                              "WR",
                              "mrPd",
                              "aPd",
                              "pMass",
                              "pMass.i",
                              "pLean",
                              "pFat",
                              "SA.wing",
                              "SA.plagio",
                              "rEWL.body",
                              "rEWL.wing",
                              "beta1",
                              "beta2",
                              "beta3",
                              "mu1",
                              "mu2"
          )

paramset<-hypercubeadj

#Determine environment to run model over
env.df  <- buildEnv(temp = c(0,20), pct.rh = c(80,100), range.res.temp = 5, range.res.rh = 5, twinter = 6, winter.res = 24)

#Create matrix of parameter values over environment
paramset.H <- env.df
sen.results.a <-matrix(NA,nrow=length(paramset[,1]),ncol=nrow(paramset.H$env))
sen.results.b <-matrix(NA,nrow=length(paramset[,1]),ncol=nrow(paramset.H$env))
sen.results.c <-matrix(NA,nrow=length(paramset[,1]),ncol=nrow(paramset.H$env))

#Run model over environment & parameter space
for (i in 1:length(paramset[,1])){

  res_out.a <- matrix(0, nrow(paramset.H$env),1)
  res_out.b <- matrix(0, nrow(paramset.H$env),1)
  res_out.c <- matrix(0, nrow(paramset.H$env),1)

  for (j in 1:nrow(paramset.H$env)) {
    #Calculate survival time
    env <- paramset.H$env[j,]
    env.df  <- buildEnv(temp = c(env$Ta,env$Ta), pct.rh = c(env$pct.rh,env$pct.rh), range.res.temp = 1, range.res.rh = 1, twinter = 10, winter.res = 24)
    res.a = hibernationModel(env = env.df, bat.params = as.list(paramset[i,]), fung.params = as.list(paramset[i,]))

    #Subset results to single values
    res.a = max(res.a$time[res.a$surv.inf == 1])

    #Calculate fungal growth rate over an hour (for use in other 2 functions)
    area = fungalGrowth(Tb = paramset.H$env[j,1], fung.params = as.list(paramset[i,]), t.min = 0)*scaleFungalGrowth(pct.rh = paramset.H$env[j,2], fung.params = as.list(paramset[i,]))

    # #Calculate EWL over an hour
     res.b = ewl(Ta=paramset.H$env[j,1], pct.rh=paramset.H$env[j,2], areaPd = area*24,  t=24, fung.params = as.list(paramset[i,]), bat.params = as.list(paramset[i,]), torpid = TRUE, WNS = TRUE)$TotalEWL
    #
    # #Calculate torpor duration at end of winter (time for Pd growth is max survival time from res.a)
    res.c = torporTime(Ta=paramset.H$env[j,1], pct.rh=paramset.H$env[j,2], WNS = TRUE, fung.params = as.list(paramset[i,]), bat.params = as.list(paramset[i,]), areaPd = area*res.a)

    #Fill in results matrix
    res_out.a[j,] <- res.a
   res_out.b[j,] <- res.b
   res_out.c[j,] <- res.c
  }

  sen.results.a[i,]<-as.vector(t(res_out.a))
  # sen.results.b[i,]<-as.vector(t(res_out.b))
  # sen.results.c[i,]<-as.vector(t(res_out.c))
}
sen.results.surv = sen.results.a
sen.results.ewl  = sen.results.b
sen.results.tbd  = sen.results.c
save(sen.results.surv, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_surv_5March2019.RData")
save(sen.results.ewl, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_ewl_27June2018.RData")
save(sen.results.tbd, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_tor_27June2018.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_surv_5March2019.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_ewl_27June2018.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_tor_27June2018.RData")

#Apply function to new model results [removed pFly because it was throwing an error and we don't care about it]
PRCCresults.a <- prcc(par.mat = paramset, model.output = sen.results.surv, routine = "blower", par.names = colnames(paramset),output.names = seq(1,ncol(sen.results.surv)))
PRCCresults.b <- prcc(par.mat = paramset, model.output = sen.results.ewl[,1:32], routine = "blower", par.names = colnames(paramset),output.names = seq(1,ncol(sen.results.ewl[,1:32])))
PRCCresults.c <- prcc(par.mat = paramset, model.output = sen.results.tbd[,1:32], routine = "blower", par.names = colnames(paramset),output.names = seq(1,ncol(sen.results.tbd[,1:32])))

#Reorganize results
df.a <- PRCCresults.a[[1]][1]
df.b <- PRCCresults.b[[1]][1]
df.c <- PRCCresults.c[[1]][1]
for(x in 2:length(PRCCresults.a)){      #Change to # in prcc results list
  df.a <- cbind(df.a,PRCCresults.a[[x]][1])
  df.b <- cbind(df.b,PRCCresults.b[[x]][1])
  df.c <- cbind(df.c,PRCCresults.c[[x]][1])
}

#Take mean prcc
means.a <- data.frame(rowMeans(df.a))
means.b <- data.frame(rowMeans(df.b))
means.c <- data.frame(rowMeans(df.c))

#Reorganize df for plotting purposes (ie remove variables that aren't within actual function)
means.a <- data.frame(means=PRCCresults.a[[1]][c(1:3,5:14,16:18,20:23,25:26),1], label = row.names(PRCCresults.a[[1]][c(1:3,5:14,16:18,20:23,25:26),]))
means.b <- data.frame(means=PRCCresults.b[[1]][c(1,3,6,12:13,18,20:23,25:26),1],label = row.names(PRCCresults.b[[1]][c(1,3,6,12:13,18,20:23,25:26),]))
means.c <- data.frame(means=PRCCresults.c[[1]][c(1,3,6,9,12:14,16,18,20:23,25:26),1],label = row.names(PRCCresults.a[[1]][c(1,3,6,9,12:14,16,18,20:23,25:26),]))
# means.all <- data.frame(rbind(means.b, means.c), Value = c(rep("EWL",length(means.c$means)), rep("TBD",length(means.c$means))))

#Plot mean with significance
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()
windowsFonts(A = windowsFont("Times New Roman"))
op <- par(family = "serif")
names.a = expression("TMR"[min],"Total Body Water Threshold",  mu[1], mu[2],"Pd effect on EWL",  "Pd effect on TMR",
                     beta[1], beta[2],  "t"[tormax], "C"[tor], "rEWL"[body],"RMR",
                     "rEWL"[wing], "t"[eu], "C"[eu], "T"[defend], "Warming Rate", "SA"[wing],"T"[tormin], "Proportion Lean Mass", "Proportion Fat Mass","M"[b])

names.b = expression(beta[1], "Pd effect on TMR", "P",beta[2], mu[1], mu[2], "TMR"[min], "T"[tormin], "M"[b], "rEWL"[body], "SA"[wing], "rEWL"[wing])
names.c = expression(mu[2], "TMR"[min], beta[2], "Total Body Water Threshold", "t"[tormax], beta[1], mu[1], "aPd", "mrPd", "T"[tormin],
                     "rEWL"[body], "SA"[wing], "rEWL"[wing], "M"[b], "Proportion Lean Mass")

means.a$fill.a = c("bat","bat","bat","bat","bat","bat","bat","effect","bat","bat",
                   "effect","bat",
                   "bat","bat","bat","bat","bat","bat",
                   "B","B",
                   "mu","mu")

t.cutoff=qt(0.05/2,df=100-2)
sig.cutoffs=c( (-(t.cutoff^2)-sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)),
               (-(t.cutoff^2)+sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)))

ggplot(means.a, aes(y =means.a[,1], x=reorder(label,abs(means.a[,1])), fill = fill.a))+
  geom_bar(stat='identity', color = "black") +
  scale_fill_manual("",values = c("bat" = "dodgerblue1", "effect" = "gold", "B" = "black", "mu" = "white")) +
  coord_flip() +
  geom_hline(yintercept = sig.cutoffs,linetype = 2, size=1.05) +
  geom_hline(yintercept = 0) +
  theme(panel.border = element_rect( fill = NA)) +
  theme(axis.text = element_text(size=14, color = "black"), axis.title=element_text(size=14)) +
  theme(text=element_text(family="A")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels = names.a) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  labs(y = "PRCC") + labs(x = "") + ylim(-1,1)

ggplot(means.b, aes(y =means.b[,1], x=reorder(label,abs(means.b[,1]))))+
  geom_bar(stat='identity', alpha=.55) +
  coord_flip() +
  geom_hline(yintercept = sig.cutoffs,linetype = 2, size=1.05) +
  geom_hline(yintercept = 0) +
  theme(panel.border = element_rect( fill = NA)) +
  theme(axis.text = element_text(size=14, color = "black"), axis.title=element_text(size=14)) +
  # theme(text=element_text(family="A")) +
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels = names.b) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  labs(y = "PRCC") + labs(x = "") + ylim(-1,1)

ggplot(means.c, aes(y =means.c[,1], x=reorder(label,abs(means.c[,1]))))+
  geom_bar(stat='identity', alpha=.55) +
  coord_flip() +
  geom_hline(yintercept = sig.cutoffs,linetype = 2, size=1.05) +
  geom_hline(yintercept = 0) +
  theme(panel.border = element_rect( fill = NA)) +
  theme(axis.text = element_text(size=14, color = "black"), axis.title=element_text(size=14)) +
  # theme(text=element_text(family="A")) +
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels = names.c) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  labs(y = "PRCC") + labs(x = "") + ylim(-1,1)

# ggplot(means.all, aes(y =means.all[,1], x=label))+
#   theme_bw() +
#   facet_wrap(~Value, ncol = 2) +
#   geom_bar(stat='identity',aes(y = means.all[,1], x =label), alpha=.55) +
#   coord_flip() +
#   geom_hline(yintercept = sig.cutoffs,linetype = 2, size=1.05) +
#   geom_hline(yintercept = 0) +
#   theme(panel.border = element_rect( fill = NA)) +
#   theme(axis.text = element_text(size=14, color = "black"), axis.title=element_text(size=14)) +
#   # theme(text=element_text(family="A")) +
#   theme(strip.text.x = element_text(size = 13)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   scale_x_discrete(limits = means.c$label, labels = names.c) +
#   theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
#   labs(y = "PRCC") + labs(x = "") + ylim(-1,1)

################################################################################
#### Fit other functions to fungal growth (use original functions)          ####
################################################################################
fung.data <- read.csv("C:/Users/Katie Haase/Desktop/Current/Pdgrowth.csv")
grwthrate <- fung.data$dhrly
area <- fung.data$Area2
WVP <- fung.data$WVP
plot(WVP, grwthrate)

#Michaelis-Menten
MM <- nls(grwthrate~mu1*WVP/(1+mu2*WVP),start=list(mu1=0.01,mu2=0.1),trace=F)
predict.MM <- coef(MM)[1] * WVP/(1 + coef(MM)[2] * WVP)

#Cubic poly
Cubic <- nls(grwthrate ~ (WVP^3) + (alpha2*(WVP^2)) + (alpha1*WVP) + alpha, start = list(alpha=0.1, alpha1=0.1, alpha2=0.1))
predict.cub = (WVP^3) + (coef(Cubic)[3]*(WVP^2)) + (coef(Cubic)[2]*WVP) + coef(Cubic)[1]


plot(WVP, grwthrate, pch = 16)
points(WVP, predict.cub, col = "red", pch = 20)
lines(WVP, predict.cub, col = "red")
points(WVP, predict.MM, col = "blue", pch = 20)
lines(WVP, predict.MM, col = "blue")

################################################################################
#### Run dynamic energy function over environmental space                   ####
################################################################################
#Read in params file
params <- read.csv("C:/Users/Katie Haase/Desktop/Current/paramUpdate_May2019.csv")

#Create dataframe of environmental parameters (taken from our microclimate data)
env.df  <- buildEnv(temp = c(-5,20), pct.rh = c(80,100), range.res.temp = 0.25, range.res.rh = 0.25, twinter = 500, winter.res = 24)

#Create vector of species
species <- params$Species
species = species[!species %in% c("MYYU","MYCA")]
species = c("MYYU", "MYCA", "LANO")

#Run model over parameter space per species
date()
# survival.figs = data.frame()
for(s in species){

  #Assign bat parameters
  s.params <- batLoad(bat.params, s)
  s.params$ttormax = 20*24
  s.params$TMRmin  = params$TMRmin[params$Species == s]
  s.params$Mass    = params$Mass[params$Species == s]
  s.params$pLean   = params$pLean[params$Species == s]
  s.params$pFat    = params$pFat[params$Species == s]
  s.params$rEWL.wing = params$rEWL.wing[params$Species == s]
  s.params$SA.wing   = params$SA.wing[params$Species == s]
  s.params$SA.body   = params$SA.body[params$Species == s]

  #Calculate dynamic energy over range of environmental conditions
  de.df1 <- data.frame(hibernationModel(env = buildEnv(temp = c(-5,20), pct.rh = c(20,40), range.res.temp = 0.25, range.res.rh = 0.25, twinter = 500, winter.res = 24),
                                       bat.params = s.params, fung.params = fung.params))
  de.df2 <- data.frame(hibernationModel(env = buildEnv(temp = c(-5,20), pct.rh = c(40.25,60), range.res.temp = 0.25, range.res.rh = 0.25, twinter = 500, winter.res = 24),
                                       bat.params = s.params, fung.params = fung.params))
  de.df3 <- data.frame(hibernationModel(env = buildEnv(temp = c(-5,20), pct.rh = c(60.25,80), range.res.temp = 0.25, range.res.rh = 0.25, twinter = 500, winter.res = 24),
                                       bat.params = s.params, fung.params = fung.params))
  de.df4 <- data.frame(hibernationModel(env = buildEnv(temp = c(-5,20), pct.rh = c(80.25,100), range.res.temp = 0.25, range.res.rh = 0.25, twinter = 500, winter.res = 24),
                                       bat.params = s.params, fung.params = fung.params))

  #Append to data frame and remove parameter names
  surv.out = rbind(de.df1, de.df2, de.df3, de.df4)
  surv.out$species <- rep(s,dim(surv.out)[1])

  save(surv.out, file = paste("C:/Users/Katie Haase/Desktop/Current/WNS susceptibility/Output/Heat Maps/survResults_",s,"_14May2019.RData", sep=""))
  print(s)
  print(date())
  # survival.figs <- rbind(survival.figs, surv.out)

}
# save(survival.figs, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/survResults_ALL_14May2019.RData")
# load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/survResults_ALL_14May2019.RData")

################################################################################
#### Plot monthly survival over parameter space                             ####
################################################################################
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/survResults_ALL_14May2019.RData")

load("survResults_MYLU_14May2019.RData")
mylu = surv.out
load("survResults_MYYU_14May2019.RData")
myyu = surv.out
load("survResults_MYCA_14May2019.RData")
myca = surv.out
load("survResults_MYTH_14May2019.RData")
myth = surv.out
load("survResults_MYEV_14May2019.RData")
myev = surv.out
load("survResults_MYVO_14May2019.RData")
myvo = surv.out
load("survResults_MYVE_14May2019.RData")
myve = surv.out
load("survResults_PESU_14May2019.RData")
pesu = surv.out
load("survResults_MYVO_14May2019.RData")
coto = surv.out
load("survResults_TABR_14May2019.RData")
tabr = surv.out
load("survResults_LANO_14May2019.RData")
lano = surv.out
load("survResults_EPFU_14May2019.RData")
epfu = surv.out
load("survResults_MYCI_14May2019.RData")
myci = surv.out

#Create vectors of environmental and species data
temps <- seq(-5,20,0.25)
hd <- seq(20,100,0.25)
species <- c("LANO", "MYCA", "MYYU")

#Organize survival data for plotting purposes
for(s in species){
  load(paste("C:/Users/Katie Haase/Desktop/Current/WNS susceptibility/Output/Heat Maps/survResults_",s,"_14May2019.RData", sep=""))
  df.months <- data.frame()

  for(t in temps){
    for(h in hd){
      s.data <- subset(surv.out, surv.out$species == s & surv.out$Ta == t & surv.out$pct.rh == h)
      months.inf  <- max(s.data$time[s.data$surv.inf == 1])/24/30
      months.null <- max(s.data$time[s.data$surv.null == 1])/24/30
      days.inf  <- max(s.data$time[s.data$surv.inf == 1])/24
      days.null <- max(s.data$time[s.data$surv.null == 1])/24
      pd.area <- s.data$Pd.growth[s.data$time == max(s.data$time[s.data$surv.inf == 1])]
      df <- data.frame(Species = s, Ta = t, pct.rh = h,
                       Months.inf = months.inf, Months.null = months.null,
                       Days.inf = days.inf, Days.null = days.null,
                       Pdarea = pd.area)
      df.months = rbind(df.months,df)
     }
  }
  save(df.months, file = paste("C:/Users/Katie Haase/Desktop/Current/WNS susceptibility/Output/Heat Maps/months4plot_",s,"_22May2019.RData", sep=""))
}

#Load data.frames
load("months4plot_MYLU_22May2019.RData")
mylu = df.months
load("months4plot_MYYU_22May2019.RData")
myyu = df.months
load("months4plot_MYCA_22May2019.RData")
myca = df.months
load("months4plot_MYTH_22May2019.RData")
myth = df.months
load("months4plot_MYEV_22May2019.RData")
myev = df.months
load("months4plot_MYVO_22May2019.RData")
myvo = df.months
load("months4plot_MYVE_22May2019.RData")
myve = df.months
load("months4plot_PESU_22May2019.RData")
pesu = df.months
load("months4plot_COTO_22May2019.RData")
coto = df.months
load("months4plot_TABR_22May2019.RData")
tabr = df.months
load("months4plot_LANO_22May2019.RData")
lano = df.months
load("months4plot_EPFU_22May2019.RData")
epfu = df.months
load("months4plot_MYCI_22May2019.RData")
myci = df.months

df.months = rbind(mylu, myyu, myca, myth, myev, myvo, myve, pesu, coto, tabr, lano, epfu, myci)

#Create functions for plotting over space
plot.data <- data.frame(Species = rep(df.months$Species,2), Ta = rep(df.months$Ta,2), pct.rh = rep(df.months$pct.rh,2),
                        Survival = c(df.months$Days.null,df.months$Days.inf),
                        Treatment = c(rep("Healthy", length(df.months$Months.inf)),rep("WNS",length(df.months$Months.inf))))
plot.data$Survival = ifelse(plot.data$Survival > 360, 360, plot.data$Survival)
plot.data$dWVP = rh2wvp(pct.rh = 100, Ta = plot.data$Ta)-rh2wvp(pct.rh = plot.data$pct.rh, Ta = plot.data$Ta)

plotEnvSpace <- function(plot.data, s, temp.lim = c(0,20), hd.lim = c(60,100), winter = 180, title){
  plot.sub <- plot.data[plot.data$Species == s, ]
  ggplot(plot.sub, aes(Ta, pct.rh, z = Survival)) +
    theme_bw() +
    facet_wrap(~Treatment, ncol = 2) +
    geom_raster(aes(fill = Survival), interpolate = TRUE, hjust = 0, vjust = 0) +
    geom_contour(bins = 12, colour = "grey15", breaks = c(0,60,90,120,150,180)) +
    geom_contour(bins = 1, colour = "grey15", breaks = winter, size = 1.5) +
    xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
    ylab("Relative Humidity (%)") +
    ggtitle(paste(title)) +
    theme(plot.title = element_text(size=18, face="bold.italic", hjust = 0.5)) +
    theme(axis.title = element_text(size = 14,  family="serif"),
          axis.text = element_text(size = 14, color = "black", family="serif"),
          aspect.ratio = 1,
          legend.key.size = unit(36, "points"),
          legend.title = element_text(size = 14,  family="serif"),
          legend.text = element_text(size = 14,  family="serif")) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(xlim = temp.lim, ylim = hd.lim) +
    theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) +
    theme(strip.text.x = element_text(size = 13)) +
    theme(panel.spacing = unit(1, "lines")) +
    theme(axis.ticks = element_line(color = "black"))+
    scale_fill_gradientn("Days", na.value = "white",
                        colours = c( "dodgerblue4","cadetblue1", "lightgoldenrod1"))
}

#Apply function to all species
p1 = plotEnvSpace(plot.data, s="MYLU", temp.lim = c(0,15), hd.lim = c(60,100), winter = 180, title = "Myotis lucifugus"); plot(p1)
p2 = plotEnvSpace(plot.data, s="MYVE", temp.lim = c(0,15), hd.lim = c(60,100), winter = 135, title = "Myotis velifer"); plot(p2)
p3 = plotEnvSpace(plot.data, s="EPFU", temp.lim = c(0,15), hd.lim = c(60,100), winter = 180, title = "Eptesicus fuscus"); plot(p3)
p4 = plotEnvSpace(plot.data, s="COTO", temp.lim = c(0,15), hd.lim = c(60,100), winter = 180, title = "Corynorhinus townsendii"); plot(p4)
p5 = plotEnvSpace(plot.data, s="PESU", temp.lim = c(0,15), hd.lim = c(60,100), winter = 135, title = "Perimyotis subflavus"); plot(p5)
p6 = plotEnvSpace(plot.data, s="MYEV", temp.lim = c(0,15), hd.lim = c(60,100), winter = 180, title = "Myotis evotis"); plot(p6)
p7 = plotEnvSpace(plot.data, s="MYVO", temp.lim = c(0,15), hd.lim = c(60,100), winter = 180, title = "Myotis volans"); plot(p7)
p8 = plotEnvSpace(plot.data, s="MYTH", temp.lim = c(0,15), hd.lim = c(60,100), winter = 180, title = "Myotis thysanodes"); plot(p8)
p9 = plotEnvSpace(plot.data, s="TABR", temp.lim = c(0,15), hd.lim = c(60,100), winter = 135, title = "Tadarida brasiliensis"); plot(p9)
p10 = plotEnvSpace(plot.data, s="MYCI", temp.lim = c(0,15), hd.lim = c(60,100), winter = 180, title = "Myotis ciliolabrum"); plot(p10)

#Plot by dWVP
plotEnvspace.wvp <- function(plot.data, s, m, site, title){
  plot.sub = plot.data[plot.data$Species == s &
                         plot.data$Ta >= 0 & plot.data$Ta <= 15,]
  plot.sub$label = ifelse(plot.sub$Treatment == "Healthy", "A", "B")
  plot.lines = rbind(data.frame(x = unique(plot.sub$Ta),
                                y = as.numeric((tapply(plot.sub$dWVP[plot.sub$Treatment == "WNS"], INDEX = plot.sub$Ta[plot.sub$Treatment == "WNS"], FUN = max))),
                                Treatment = rep("WNS", length(unique(plot.sub$Ta)))),
                     data.frame(x = unique(plot.sub$Ta),
                                y = as.numeric((tapply(plot.sub$dWVP[plot.sub$Treatment == "Healthy"], INDEX = plot.sub$Ta[plot.sub$Treatment == "Healthy"], FUN = max))),
                                Treatment = rep("Healthy", length(unique(plot.sub$Ta)))))
  plot.lines$y1 = plot.lines$y + 0.005

  winter.lines  = rbind(data.frame(x = unique(plot.sub$Ta[plot.sub$Survival >= m & plot.sub$Treatment == "WNS"]),
                                   y = as.numeric((tapply(plot.sub$dWVP[plot.sub$Treatment == "WNS" & plot.sub$Survival >= m], INDEX = plot.sub$Ta[plot.sub$Treatment == "WNS"& plot.sub$Survival >= m], FUN = max))),
                                   Treatment = rep("WNS", length(unique(plot.sub$Ta[plot.sub$Survival >= m & plot.sub$Treatment == "WNS"])))),
                        data.frame(x = unique(plot.sub$Ta[plot.sub$Survival >= m & plot.sub$Treatment == "Healthy"]),
                                   y = as.numeric((tapply(plot.sub$dWVP[plot.sub$Treatment == "Healthy"& plot.sub$Survival >= m], INDEX = plot.sub$Ta[plot.sub$Treatment == "Healthy"& plot.sub$Survival >= m], FUN = max))),
                                   Treatment = rep("Healthy", length(unique(plot.sub$Ta[plot.sub$Survival >= m & plot.sub$Treatment == "Healthy"])))))
  winter.lines = rbind(data.frame(x= min(winter.lines$x[winter.lines$Treatment == "WNS"])-0.25, y = 0, Treatment = "WNS"),
                       data.frame(x= max(winter.lines$x[winter.lines$Treatment == "WNS"])+0.25, y = 0, Treatment = "WNS"),
                       data.frame(x= min(winter.lines$x[winter.lines$Treatment == "Healthy"])-0.25, y = 0, Treatment = "Healthy"),
                       data.frame(x= max(winter.lines$x[winter.lines$Treatment == "Healthy"])+0.25, y = 0, Treatment = "Healthy"),
                       winter.lines)

  polygon = data.frame(x = c(mc.summary$Min.Temp[mc.summary$uniqueid == site],
                             mc.summary$Min.Temp[mc.summary$uniqueid == site],
                             mc.summary$Max.Temp[mc.summary$uniqueid == site],
                             mc.summary$Max.Temp[mc.summary$uniqueid == site]),
                       y = c(dwvp(Ta = mc.summary$Min.Temp[mc.summary$uniqueid == site], pct.rh = mc.summary$Max.RH[mc.summary$uniqueid == site]),
                             dwvp(Ta = mc.summary$Min.Temp[mc.summary$uniqueid == site], pct.rh = mc.summary$Min.RH[mc.summary$uniqueid == site]),
                             dwvp(Ta = mc.summary$Max.Temp[mc.summary$uniqueid == site], pct.rh = mc.summary$Min.RH[mc.summary$uniqueid == site]),
                             dwvp(Ta = mc.summary$Max.Temp[mc.summary$uniqueid == site], pct.rh = mc.summary$Max.RH[mc.summary$uniqueid == site])))

ggplot(data = plot.sub, aes(x = Ta, y = dWVP)) +
  theme(panel.border = element_rect(fill = NA)) +
  theme_bw() +
  ggtitle(paste(title)) +
  theme(plot.title = element_text(face = "italic")) +
  facet_wrap(~Treatment, ncol = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point(size=2, aes(color = Survival)) +
  geom_line(data = plot.lines, aes(x=x, y=y1), size = 2.5, color = "white") +
  geom_line(data = winter.lines, aes(x=x, y=y), color = "black") +
  theme(panel.spacing = unit(1, "lines")) +
  geom_polygon(data = polygon, aes(x = x, y=y), color = "black",fill = NA, linetype = "dashed") +
  # xlab("") + ylab("") +
  xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
  ylab("Water Pressure Deficit (kPa)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10, color = "black")) +
        # aspect.ratio = 1,
        # legend.key.size = unit(36, "points"),
        # legend.title = element_text(size = 12,  family="serif"),
        # legend.text = element_text(size = 12,  family="serif")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0,15)) +
  theme(axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 13)) +
  theme(axis.ticks = element_line(color = "black"))+
  # geom_text(aes(label = label, group = Treatment, x = -Inf, y = Inf, hjust = -.5, vjust = 1.25, fontface = "bold"), size = 5) +
  # geom_text(aes(label = "winter = 181 days",  x = -Inf, y = Inf, hjust = -.05, vjust = 1.25), size = 3) +
  scale_color_gradientn("Days until \nFat Exhaustion",
                       colours = c("dodgerblue4","cadetblue1", "lightgoldenrod1"))
}
p9 = plotEnvspace.wvp(plot.data, s = "COTO", m = 147, site = "BCM_MID", title = "Corynorhinus townsendii")
p5 = plotEnvspace.wvp(plot.data, s = "EPFU", m = 180, site = "ODW_ENTRANCE",title = "Eptesicus fuscus")
p13 = plotEnvspace.wvp(plot.data, s = "LANO", m = 180, site = "QVM_UPPER", title = "Lasionycteris noctivagans")
p10 = plotEnvspace.wvp(plot.data, s = "MYCI", m = 147, site = "BCM_MID", title = "Myotis ciliolabrum")
p8 = plotEnvspace.wvp(plot.data, s = "TABR", m = 134, site = "CLT_INSIDE", title = "Tadarida brasiliensis")

p11 = plotEnvspace.wvp(plot.data, s = "MYCA", m = 180, site = "QVM_UPPER",title = "Myotis californicus")
p3 = plotEnvspace.wvp(plot.data, s = "MYEV", m = 180, site = "LCC_CATHEDRAL",title = "Myotis evotis")
p1 = plotEnvspace.wvp(plot.data, s = "MYLU" ,m = 180, site = "LCC_CATHEDRAL",title = "Myotis lucifugus")
p4 = plotEnvspace.wvp(plot.data, s = "MYTH", m = 180, site = "LCC_CATHEDRAL",title = "Myotis thysanodes")
p6 = plotEnvspace.wvp(plot.data, s = "MYVE", m = 134, site = "SLL_SHALLOW", title = "Myotis velifer")
p2 = plotEnvspace.wvp(plot.data, s = "MYVO", m = 180, site = "LCC_CATHEDRAL",title = "Myotis volans")
p12 = plotEnvspace.wvp(plot.data, s = "MYYU", m = 180, site = "CAC_INSIDE", title = "Myotis yumanensis")
p7 = plotEnvspace.wvp(plot.data, s = "PESU", m = 134, site = "SLL_SHALLOW", title = "Perimyotis subflavus")


multiplot(p9,p13,p8,p7,p5,p10,p7,p7, cols = 2)
multiplot(p11, p1, p6, p12,p3,p4,p2,p7, cols = 2)




load("survResults_TABR_14May2019.RData")
surv.out$fatloss = surv.out$g.fat.consumed/surv.out$time*24
surv.out$fatloss.null = surv.out$n.g.fat.consumed/surv.out$time*24

summary(df.months$Days.null[df.months$Species == "TABR" &
                             df.months$Ta <= 15 & df.months$Ta >= 0 &
                             df.months$pct.rh >= 60])
summary(df.months$Days.inf[df.months$Species == "TABR" &
                               df.months$Ta <= 15 & df.months$Ta >= 0 &
                               df.months$pct.rh >= 60])

summary(surv.out$fatloss.null[surv.out$time == 14*24 &
                                surv.out$Ta <= 15 & surv.out$Ta >= 0 &
                         surv.out$pct.rh >= 60])
summary(surv.out$fatloss[surv.out$time == 14*24 &
                           surv.out$Ta <= 15 & surv.out$Ta >= 0 &
                         surv.out$pct.rh >= 60])
summary(surv.out$fatloss[surv.out$time == 135*24 &
                           surv.out$Ta <= 15 & surv.out$Ta >= 0 &
                         surv.out$pct.rh >= 60])




results = read.csv("C:/Users/Katie Haase/Desktop/Current/WNS susceptibility/Input/results.csv")

results$orderedSpecies = with(results, reorder(Species, Days.Inf))
results$orderedSpecies2 = with(results, reorder(Species, Rate.14))

p1 = ggplot(results, aes(x = orderedSpecies, y = Days.Inf)) +
  theme_bw() +
  theme(panel.border = element_rect(fill = NA)) +
  geom_errorbar(aes(x = orderedSpecies, ymin=Days.Inf.L, ymax=Days.Inf.H), width=.2) +
  geom_point(aes(x = orderedSpecies, y = Days.Inf), size = 3) +
  coord_flip() +
  xlab("") +
  ylab("Days Until Fat Exhaustion") +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm"),face = "italic"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm"))) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(results, aes(x = orderedSpecies2, y = Rate.14)) +
  theme_bw() +
  theme(panel.border = element_rect(fill = NA)) +
  geom_errorbar(aes(x = orderedSpecies2, ymin=Rate.14.L, ymax=Rate.14.H), width=0) +
  geom_point(aes(x = orderedSpecies2, y = Rate.14), size = 3) +
  geom_errorbar(aes(x = orderedSpecies2, ymin=Rate.End.L, ymax=Rate.End.H), color = "grey65", width=0) +
  geom_point(aes(x = orderedSpecies2, y = Rate.End), color = "grey65", size = 3) +
  geom_errorbar(aes(x = orderedSpecies2, ymin=Rate.Null.L, ymax=Rate.Null.H), color = "grey35", width=0) +
  geom_point(aes(x = orderedSpecies2, y = Rate.Null), color = "grey35", size = 3) +
  coord_flip() +
  xlab("") +
  ylab("Rate of Fat Loss") +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm"),face = "italic"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm"))) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

per.df = data.frame()
for(s in unique(df.months$Species)){
  siteid <- if(s == "MYLU"){
    "LCC_CATHEDRAL"
  }else if(s == "COTO"){
    "BCM_MID"
  }else if(s == "MYCI"){
    "BCM_MID"
  }else if(s == "EPFU"){
    "ODW_ENTRANCE"
  }else if(s == "MYEV"){
    "LCC_CATHEDRAL"
  }else if(s == "MYTH"){
    "LCC_CATHEDRAL"
  }else if(s == "MYVE"){
    "SLL_SHALLOW"
  }else if(s == "MYVO"){
    "LCC_CATHEDRAL"
  }else if(s == "PESU"){
    "SLL_SHALLOW"
  }else if(s == "TABR"){
    "CLT_INSIDE"
  }else if(s == "MYCA"){
    "QVM_UPPER"
  }else if(s == "MYYU"){
    "CAC_INSIDE"
  }else if(s == "LANO"){
    "QVM_UPPER"
  }

  dur <- if(s == "MYLU"){
    183.5209045
  }else if(s == "COTO"){  #Run separately for multiple sites?
    147.1204834
  }else if(s == "MYCI"){
    147.1204834
  }else if(s == "EPFU"){
    183.5209045
  }else if(s == "MYEV"){
    183.5209045
  }else if(s == "MYTH"){
    183.5209045
  }else if(s == "MYVE"){
    134.458541870117
  }else if(s == "MYVO"){
    183.5209045
  }else if(s == "PESU"){
    134.458541870117
  }else if(s == "TABR"){
    134.458541870117
  }else if(s == "MYCA"){
    183.5209045
  }else if(s == "MYYU"){
    183.5209045
  }else {183.5209045}


  df = df.months[df.months$Species == s &
              df.months$Ta <= mc.sites$Max.Temp[match(siteid, mc.sites$uniqueid)] &
              df.months$Ta >= mc.sites$Min.Temp[match(siteid, mc.sites$uniqueid)] &
              df.months$pct.rh >= mc.sites$Min.RH[match(siteid, mc.sites$uniqueid)],]
  per = dim(df[df$Days.inf>= dur,])[1]/dim(df)[1]
  per.df = rbind(per.df, data.frame(Species = s, Percent = per))
}


################################################################################
#### Trade-offs figures                                                     ####
################################################################################
#Calculate rate of fat loss instead of exact time


#Plot
plot.data <- data.frame(Species = rep(df.months$Species,2), Ta = rep(df.months$Ta,2), pct.rh = rep(df.months$pct.rh,2),
                        Rate = c(df.months$rate.null,df.months$rate.inf),
                        Treatment = c(rep("Healthy", length(df.months$rate.inf)),rep("Pd-infected",length(df.months$rate.inf))))

ggplot(plot.data[plot.data$pct.rh == 100 & plot.data$Ta >= 0,], aes(x = Ta, y = Rate)) +
  theme_bw() +
  facet_wrap(~Treatment, ncol = 2) +
  geom_line(aes(x = Ta, y = Rate),  size = 1.2, color = "dodgerblue2") +
  geom_line(data = plot.data[plot.data$pct.rh == 90 & plot.data$Ta >= 0,], aes(x = Ta, y = Rate), size = 1.1, linetype = 2) +
  theme(panel.border = element_rect(fill = NA, color = "black")) +
  xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
  ylab(expression(paste("Rate of Fat Loss (g",phantom(0), day^-1,")"))) +
  theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0))) +
  theme(axis.title = element_text(size = 13,  family="serif"),
        axis.text = element_text(size = 12, color = "black",  family="serif")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlim(0,20) +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm")))

ggplot(plot.data[plot.data$Ta == 5,], aes(x = pct.rh, y = Rate)) +
  facet_wrap(~Treatment, ncol = 2) +
  geom_line(aes(x = pct.rh, y = Rate),  size = 1) +
  geom_line(data = plot.data[plot.data$Ta == 15,], aes(x = pct.rh, y = Rate), size = 1, linetype = 2) +
  theme(panel.border = element_rect( fill = NA, color = "black")) +
  xlab("Relative Humidity (%)") +
  ylab(expression(paste("Rate of Fat Loss (g",phantom(0), day^-1,")"))) +
  theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0))) +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme(axis.title = element_text(size = 14,  family="serif"),
        axis.text = element_text(size = 12, color = "black",  family="serif")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm")))

#Plot differences between 100% and other humidities
plot.diff <- data.frame(Ta = unique(plot.data$Ta))
plot.diff$null.90 <- plot.data$Rate[plot.data$Treatment == "Healthy" & plot.data$pct.rh == 90] -
                     plot.data$Rate[plot.data$Treatment == "Healthy" & plot.data$pct.rh == 97]
plot.diff$inf.90  <- plot.data$Rate[plot.data$Treatment == "Pd-infected" & plot.data$pct.rh == 90] -
                     plot.data$Rate[plot.data$Treatment == "Pd-infected" & plot.data$pct.rh == 97]
plot.diff$null.95 <- plot.data$Rate[plot.data$Treatment == "Healthy" & plot.data$pct.rh == 95] -
                     plot.data$Rate[plot.data$Treatment == "Healthy" & plot.data$pct.rh == 100]
plot.diff$inf.95  <- plot.data$Rate[plot.data$Treatment == "Pd-infected" & plot.data$pct.rh == 95] -
                     plot.data$Rate[plot.data$Treatment == "Pd-infected" & plot.data$pct.rh == 100]
plot.diff <- plot.diff[plot.diff$Ta >= 0 & plot.diff$Ta <= 15,]

plot.diff$inf.95[plot.diff$Ta < 2] = plot.diff$inf.95[plot.diff$Ta == 2] + 0.001
plot.diff$inf.95[plot.diff$Ta < 2] = plot.diff$inf.95[plot.diff$Ta == 2] + 0.001
plot.diff$inf.95[plot.diff$Ta > 14 ] = plot.diff$inf.95[plot.diff$Ta == 14]

ggplot(plot.diff) +
  theme_bw() +
  theme(panel.border = element_rect( fill = NA, color = "black")) +
  geom_line(aes(x = Ta, y = null.95), color = "dodgerblue2", size = 1.125) +
  geom_line(aes(x = Ta, y = inf.95 ), color = "black" , size = 1.125, lineend = "round") +
  geom_hline(yintercept = 0, linetype = 2, size = 1, color = "grey40") +
  theme(axis.title = element_text(size = 14,  family="serif"),
        axis.text = element_text(size = 12, color = "black",  family="serif")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm"))) +
  scale_x_continuous(expand = c(0,0)) +
  ylim(-0.06, 0.01) +
  ylab(expression(paste(Delta, " Rate of Fat Loss (g",phantom(0), day^-1,")"))) +
  xlab(expression(paste("Temperature ", degree,"C")))

################################################################################
#### Plot days of euthermia                                                 ####
################################################################################

#Cut to just 180 days
euth.df <- de.df[de.df$time == 180*24,]

#Calculate leftover fat
euth.df$fat.null <- ifelse(((s.params$Mass*s.params$pFat)-euth.df$n.g.fat.consumed)<0,0,((s.params$Mass*s.params$pFat)-euth.df$n.g.fat.consumed))
euth.df$fat.inf  <- ifelse(((s.params$Mass*s.params$pFat)-euth.df$g.fat.consumed)<0,0,((s.params$Mass*s.params$pFat)-euth.df$g.fat.consumed))

#Calculate one day of fat loss with FMR
FMR =  (((1.87+0.732*(log(s.params$Mass)))*1000)/37.6)/1000

#Calculate amount of days available with leftover fat (plus 1 day because we considered the energy of one day in the calculation of fat loss in the survival function)
euth.df$post.days.null <- (euth.df$fat.null/FMR) + 1
euth.df$post.days.inf  <- (euth.df$fat.inf/FMR) + 1

#Summarize for manuscript
summary(euth.df$post.days.null)
summary(euth.df$post.days.inf)

mean(euth.df$post.days.null[euth.df$Ta >= 4.5 & euth.df$Ta <= 6.5 & euth.df$pct.rh >= 90])
sd(euth.df$post.days.null[euth.df$Ta >= 4.5 & euth.df$Ta <= 6.5 & euth.df$pct.rh >= 90])
mean(euth.df$post.days.inf[euth.df$Ta >= 4.5 & euth.df$Ta <= 6.5 & euth.df$pct.rh >= 90])
sd(euth.df$post.days.inf[euth.df$Ta >= 4.5 & euth.df$Ta <= 6.5 & euth.df$pct.rh >= 90])

mean(euth.df$post.days.null[euth.df$pct.rh == 100])
sd(euth.df$post.days.null[euth.df$pct.rh  == 100])
mean(euth.df$post.days.inf[euth.df$pct.rh == 100])
sd(euth.df$post.days.inf[euth.df$pct.rh == 100])

mean(euth.df$post.days.null[euth.df$Ta >= 0 & euth.df$Ta <= 19 & euth.df$pct.rh  == 100])
sd(euth.df$post.days.null[euth.df$Ta >= 0 & euth.df$Ta <= 19 & euth.df$pct.rh  == 100])
mean(euth.df$post.days.inf[euth.df$Ta >= 0 & euth.df$Ta <= 19 & euth.df$pct.rh  == 100])
sd(euth.df$post.days.inf[euth.df$Ta >= 0 & euth.df$Ta <= 19 & euth.df$pct.rh  == 100])

mean(euth.df$post.days.inf[euth.df$Ta  == 20 & euth.df$pct.rh  == 100])
mean(euth.df$post.days.inf[euth.df$Ta  == 20  & euth.df$pct.rh  == 98])

#Plot heat maps to match above
plot.euth <- data.frame(Species = rep("MYLU", length(euth.df$post.days.null)*2), Ta = rep(euth.df$Ta,2), pct.rh = rep(euth.df$pct.rh,2),
                        Euthermia = c(euth.df$post.days.null, euth.df$post.days.inf),
                        Treatment = c(rep("Healthy", length(euth.df$post.days.null)),rep("WNS",length(euth.df$post.days.null))))

ggplot(plot.euth, aes(Ta, pct.rh)) +
  theme_bw() +
  facet_wrap(~Treatment, ncol = 2) +
  geom_raster(aes(fill = Euthermia), interpolate = TRUE) +
  geom_rect(aes(xmin = 4, xmax = 6, ymin = 90,  ymax = 100), linetype = 2, color = "black", fill = "NA") +
  theme(panel.border = element_rect( fill = NA, color = "black")) +
  xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
  ylab("Relative Humidity (%)") +
  theme(axis.title = element_text(size = 14),#  family="serif"),
        axis.text = element_text(size = 14, color = "black"),  # family="serif"),
        aspect.ratio = 1,
        legend.key.size = unit(36, "points"),
        legend.title = element_text(size = 14),#  family="serif"),
        legend.text = element_text(size = 14)) + #,  family="serif")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 13)) +
  theme(panel.spacing = unit(1, "lines")) +
  theme(axis.ticks = element_line(color = "black"))+
  scale_fill_gradientn("Days", colours = c("dodgerblue4","cadetblue1", "lightgoldenrod1"))

################################################################################
#### Plot by WVP                                                            ####
################################################################################

#Plot by dWVP instead
rh2wvp <- function(pct.rh, Ta){
  wvp = (pct.rh*0.01)*(0.61 * exp((17.50 * Ta)/(Ta + 240.97)))
  return(wvp)
}

plot.euth$WVP <- round(rh2wvp(pct.rh = plot.euth$pct.rh, Ta = plot.euth$Ta),2)
plot.euth$dWVP <- round(rh2wvp(pct.rh = 100, Ta = plot.euth$Ta) - plot.euth$WVP,2)
plot.euth = plot.euth[plot.euth$Species == "MYLU",]
ggplot(plot.euth, aes(Ta, dWVP)) +
  theme_bw() +
  facet_wrap(~Treatment, ncol = 2) +
  geom_raster(aes(fill = Survival), interpolate = TRUE) +
  # geom_rect(aes(xmin = 4, xmax = 6, ymin = 90,  ymax = 100), linetype = 2, color = "black", fill = "NA") +
  theme(panel.border = element_rect( fill = NA, color = "black")) +
  xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
  ylab("Relative Humidity (%)") +
  theme(axis.title = element_text(size = 14),#  family="serif"),
        axis.text = element_text(size = 14, color = "black"),  # family="serif"),
        aspect.ratio = 1,
        legend.key.size = unit(36, "points"),
        legend.title = element_text(size = 14),#  family="serif"),
        legend.text = element_text(size = 14)) + #,  family="serif")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 13)) +
  theme(panel.spacing = unit(1, "lines")) +
  theme(axis.ticks = element_line(color = "black"))+
  scale_fill_gradientn("Days", colours = c("dodgerblue4","cadetblue1", "lightgoldenrod1"))




################################################################################
#### Plot Pd growth over parameter space                                    ####
################################################################################
#Load fungal growth
fung.params <- fungalSelect("Chaturvedi")

#Create dataframe of environmental parameters (taken from our microclimate data)
env.df  <- buildEnv(temp = c(0,20), pct.rh = c(80,100), range.res.temp = 1, range.res.rh = 1, twinter = 12, winter.res = 24)

#Calculate fungal growth over environmental parameters
fung.growth = fungalGrowth(Tb = env.df$env$Ta, fung.params = fung.params, t.min = 0)*scaleFungalGrowth(pct.rh = env.df$env$pct.rh, fung.params = fung.params)*24*30
df <- data.frame(Ta = env.df$env$Ta, pct.rh = env.df$env$pct.rh, fung.growth)

p <- ggplot(df[df$pct.rh >=80 & df$Ta >=0,], aes(Ta, pct.rh)) + theme_classic() +
  theme(panel.border = element_rect( fill = NA)) +
  geom_raster(aes(fill = fung.growth), interpolate = T) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  xlab("Temperature (C)") +
  ylab("Relative Humidity (%)") +
  theme(plot.title = element_text(size = 18,  family="serif"),
        axis.title = element_text(size = 16,  family="serif"),
        axis.text = element_text(size = 16,  family="serif"),
        aspect.ratio = 1,
        legend.key.size = unit(42, "points"),
        legend.title = element_text(size = 16,  family="serif"),
        legend.text = element_text(size = 16,  family="serif")) +
  #ylim(80,100) + xlim(0,20) +
  scale_fill_gradientn(expression(paste("Monthly Fungal Growth (",cm^2,")")),colours = c("dodgerblue4","cadetblue1", "lightgoldenrod1"))
plot(p)

df2 = data.frame(Ta = env.df$env$Ta, pct.rh = env.df$env$pct.rh, growth = fungalGrowth(Tb = env.df$env$Ta, fung.params = fung.params, t.min = 0)*scaleFungalGrowth(pct.rh = env.df$env$pct.rh, fung.params = fung.params)*24)

hd.60 <- subset(df2, df2$pct.rh == 60)
hd.95 <- subset(df2, df2$pct.rh == 95)
hd.90 <- subset(df2, df2$pct.rh == 90)
hd.100 <- subset(df2, df2$pct.rh == 100)
df2 <- rbind(hd.60, hd.90, hd.95, hd.100)
df2$pct.rh <- as.factor(df2$pct.rh)

p <- ggplot(df2, aes(x=Ta, y=growth, group = pct.rh, color = pct.rh)) +
  geom_line(size =1) +
  scale_colour_manual("Humidity (%)",
                      breaks = c("100", "95", "90", "60"),
                      values = c("100"="dodgerblue1", "95"="gold", "90" = "purple", "60" = "blue")) +
  theme_classic() +
  theme(panel.border = element_rect( fill = NA)) +
  scale_x_continuous(expand = c(0.01,0.01))+
  scale_y_continuous(expand = c(0,0.01))+
  xlab("Temperature (C)") +
  ylab(expression(paste("Daily Fungal Growth Rate (  ",cm^2,")"))) +
  theme(axis.title = element_text(size = 14,  family="serif"),
        axis.text = element_text(size = 14, color = "black",  family="serif"),
        legend.title = element_text(size = 14,  family="serif"),
        legend.text = element_text(size = 14,  family="serif"))
plot(p)



################################################################################
#### Plot EWL and torpor duration over parameter space                      ####
################################################################################
#Create dataframe of environmental parameters (taken from our microclimate data)
env.df  <- buildEnv(temp = c(-5,20), pct.rh = c(60,100), range.res.temp = 1, range.res.rh = 1, twinter = 12, winter.res = 24)


#Create vector of species
species <- c("MYLU", "MYVE", "COTO", "EPFU", "PESU")

#Run EWL/TBD models over parameter space per species
#for(s in species){

  #Assign bat parameters
 # s.params <- batLoad(bat.params, s)

  s.params <- batLoad(bat.params, "MYLU")
  s.params$ttormax = 1300

  s.out = data.frame()

  for(t in env.df$twinter){
    #Calculate Pd growth over range of environmental conditions
    areaPd <- fungalGrowth(Tb = env.df$env$Ta, fung.params = fung.params, t.min = 0)*scaleFungalGrowth(pct.rh = env.df$env$pct.rh, fung.params = fung.params)*t

    #Calculate EWL over range of environmental conditions
    ewl.inf  = ewl(Ta = env.df$env$Ta, pct.rh = env.df$env$pct.rh, t = t, areaPd = areaPd, fung.params = fung.params, bat.params = s.params, torpid = TRUE, WNS = TRUE)
    ewl.null = ewl(Ta = env.df$env$Ta, pct.rh = env.df$env$pct.rh, t = t, areaPd = areaPd, fung.params = fung.params, bat.params = s.params, torpid = TRUE, WNS = FALSE)

    #Calculate torpor bout duration over range of environmental conditions
    tbd.inf  = torporTime(Ta = env.df$env$Ta, pct.rh = env.df$env$pct.rh, areaPd = areaPd, WNS = TRUE, bat.params = s.params, fung.params = fung.params)
    tbd.null = torporTime(Ta = env.df$env$Ta, pct.rh = env.df$env$pct.rh, areaPd = areaPd, WNS = FALSE, bat.params = s.params, fung.params = fung.params)

    #Append to data.frame
    s.out <- rbind(s.out, data.frame(Species = s, Time = t, Ta = ewl.inf$Ta, pct.rh = ewl.inf$pct.rh, areaPd = areaPd, TotalEWL.inf = ewl.inf$TotalEWL, CEWL.inf = ewl.inf$CutaneousEWL, PEWL.inf = ewl.inf$PulmonaryEWL,
                        TBD.inf = tbd.inf, TotalEWL.null = ewl.null$TotalEWL, CEWL.null = ewl.null$CutaneousEWL, PEWL.null = ewl.null$PulmonaryEWL, TBD.null = tbd.null))
  }
  save(s.out, file = paste("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/ewl&tbd_mylu_31May2019.RData", sep = ""))
#}

load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/ewl&tbd_mylu_5July2018.RData")

#Plot over space
mylu.EWL <- s.out[s.out$Species == "MYLU",]
plot.EWL <- data.frame(Ta = rep(mylu.EWL$Ta, 2),
                       pct.rh = rep(mylu.EWL$pct.rh,2),
                       TEWL = c(mylu.EWL$TotalEWL.null, mylu.EWL$TotalEWL.inf),
                       Time = rep(mylu.EWL$Time, 2),
                       Treatment = c(rep("Healthy", length(mylu.EWL$Ta)), rep("Pd-infected", length(mylu.EWL$Ta))))
plot.EWL <- plot.EWL[plot.EWL$pct.rh >= 80 & plot.EWL$Ta <= 15 & plot.EWL$Ta >=0,]
plot.EWL$TEWL = plot.EWL$TEWL/plot.EWL$Time


ggplot(plot.EWL[plot.EWL$Time == 24*14,], aes(Ta, pct.rh)) +
  theme_bw() +
  facet_wrap(~Treatment) +
  geom_raster(aes(fill = TEWL/8.5), interpolate = TRUE) +
  theme(panel.border = element_rect( fill = NA)) +
  # geom_rect(aes(xmin = 4, xmax = 6, ymin = 90,  ymax = 100), linetype = 2, color = "black", fill = "NA") +
  xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
  ylab("Relative Humidity (%)") +
  theme(axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 14, color = "black"),
        aspect.ratio = 1,
        legend.key.size = unit(30, "points"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  theme(strip.text.x = element_text(size = 13)) +
  theme(panel.spacing = unit(1, "lines")) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_gradientn(expression(paste("EWL (mg ",h^-1,")")), colours = c("darkgoldenrod1", "lightgoldenrod1","cadetblue1", "dodgerblue4"))  +
  theme(plot.margin=unit(c(1,2,2,1.2),"cm"))

mylu.EWL <- s.out[s.out$Species == "MYLU",]
mylu.EWL$rateEWL.null <- mylu.EWL$TotalEWL.null/mylu.EWL$Time
mylu.EWL$rateEWL.inf  <- mylu.EWL$TotalEWL.inf/mylu.EWL$Time
mylu.EWL$rate.diff <- mylu.EWL$rateEWL.inf - mylu.EWL$rateEWL.null

diff.plot <- data.frame(Ta = rep(mylu.EWL$Ta[mylu.EWL$Time == 24 * 180], 2),
                        pct.rh = rep(mylu.EWL$pct.rh[mylu.EWL$Time == 24 * 180],2),
                        diff = c(mylu.EWL$rate.diff[mylu.EWL$Time == 24], mylu.EWL$rate.diff[mylu.EWL$Time == 24 * 180]),
                        Time = c(rep("24", length(mylu.EWL$Ta[mylu.EWL$Time == 24])), rep("180", length(mylu.EWL$Ta[mylu.EWL$Time == 24]))),
                        Treatment = c(rep("Healthy", length(mylu.EWL$Ta[mylu.EWL$Time == 24])), rep("Pd-infected", length(mylu.EWL$Ta[mylu.EWL$Time == 24]))))

ggplot(diff.plot, aes(Ta, pct.rh)) +
  theme_bw() +
  facet_wrap(~Time) +
  geom_raster(aes(fill = diff), interpolate = TRUE) +
  theme(panel.border = element_rect( fill = NA)) +
  # geom_rect(aes(xmin = 4, xmax = 6, ymin = 90,  ymax = 100), linetype = 2, color = "black", fill = "NA") +
  xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
  ylab("Relative Humidity (%)") +
  theme(axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 14, color = "black"),
        aspect.ratio = 1,
        legend.key.size = unit(30, "points"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  theme(strip.text.x = element_text(size = 13)) +
  theme(panel.spacing = unit(1, "lines")) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_gradientn(expression(paste(Delta, "EWL (mg ",h^-1,")")), colours = c("darkgoldenrod1", "lightgoldenrod1","cadetblue1", "dodgerblue4"))  +
  theme(plot.margin=unit(c(1,2,2,1.2),"cm"))

mylu.EWL <- s.out[s.out$Species == "MYLU",]
mylu.EWL$rateEWL.null <- mylu.EWL$TotalEWL.null/mylu.EWL$Time
mylu.EWL$rateEWL.inf  <- mylu.EWL$TotalEWL.inf/mylu.EWL$Time

#Plot PEWL & CEWL
mylu.EWL <- s.out[s.out$Species == "MYLU",]
plot.EWL <- data.frame(Ta = rep(mylu.EWL$Ta, 2), pct.rh = rep(mylu.EWL$pct.rh,2), TEWL = c(mylu.EWL$TotalEWL.null, mylu.EWL$TotalEWL.inf),
                       CEWL = c(mylu.EWL$CEWL.null, mylu.EWL$CEWL.inf), PEWL = c(mylu.EWL$PEWL.null, mylu.EWL$PEWL.inf),
                       Treatment = c(rep("Healthy", length(mylu.EWL$Ta)), rep("Pd-infected", length(mylu.EWL$Ta))))
plot.EWL.95 <- plot.EWL[plot.EWL$pct.rh==98,]
plot.EWL.95$TEWL = plot.EWL.95$TEWL/14

ggplot(plot.EWL.95, aes(x=Ta, y=CEWL)) +
  facet_wrap(~Treatment, ncol = 2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.border = element_rect( fill = NA)) +
  #geom_line(aes(x=Ta, y=CEWL), size = 1) +
  #geom_line(aes(x=Ta, y=PEWL),size =1.125) +
  geom_ribbon(data=plot.EWL.95, aes(ymin=0,ymax=CEWL), fill="cadetblue1", alpha = .65) +
  geom_ribbon(data=plot.EWL.95, aes(ymin=0,ymax=PEWL), fill="lightgoldenrod1") +
  theme(axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 14, color = "black")) +
  labs(y = expression(paste("Evaporative Water Loss (mg",phantom(1),day^-1,")"))) + labs(x = expression(paste("Temperature (",degree,phantom(),C,")"))) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(limits = c(0,2000), expand = c(0,0)) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  #theme(strip.background =element_rect(fill="slategray1")) +
  theme(strip.text.x = element_text(size = 13)) +
  theme(panel.spacing = unit(1, "lines")) +
  theme(plot.margin=unit(c(1,1,2,1.2),"cm"))

#Plot torpor bout duration
plot.TBD <-  s.out[s.out$Species == "MYLU" & s.out$Time == 24,]
plot.TBD <-  data.frame(Ta = rep(plot.TBD$Ta,2),
                        pct.rh = rep(plot.TBD$pct.rh,2),
                        TBD = c(plot.TBD$TBD.null, plot.TBD$TBD.inf),
                        Treatment = c(rep("Healthy", length(plot.TBD$Ta)), rep("WNS", length(plot.TBD$Ta))))
plot.TBD$TBD <- plot.TBD$TBD/24
plot.TBD$Contour <- ifelse(plot.TBD$TBD <= 7, 7, ifelse(plot.TBD$TBD <= 14,14,30))

ggplot(plot.TBD[plot.TBD$Ta <= 15 & plot.TBD$Ta >= 0 & plot.TBD$pct.rh >= 80,], aes(Ta, pct.rh)) +
  theme_bw() +
  facet_wrap(~Treatment) +
  geom_raster(aes(fill = TBD), interpolate = TRUE) +
  # stat_contour(bins = 3, color = "grey10") +
  theme(panel.border = element_rect( fill = NA)) +
  #geom_rect(aes(xmin = 4, xmax = 6, ymin = 90,  ymax = 100), linetype = 2, color = "black", fill = "NA") +
  xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
  ylab("Relative Humidity (%)") +
  theme(axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 14, color = "black"),
        aspect.ratio = 1,
        legend.key.size = unit(30, "points"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  theme(strip.text.x = element_text(size = 13)) +
  theme(panel.spacing = unit(1, "lines")) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_gradientn("TBD (days)", colours = c("dodgerblue4","cadetblue1","lightgoldenrod1", "darkgoldenrod1")) +
  theme(plot.margin=unit(c(1,2.9,2,1.2),"cm"))

#Plot TBD over diff conditions - k-constant
ggplot(s.out)+
  theme_bw() +
  geom_line(data = s.out[s.out$Ta == 3 & s.out$pct.rh == 100,],
            aes(x=Time/24, y = TBD.null/24),
            size = 1, linetype = "solid", color = "goldenrod") +
  geom_line(data = s.out[s.out$Ta == 6 & s.out$pct.rh == 100,],
            aes(x=Time/24, y = TBD.null/24),
            size = 1, linetype = "dashed", color = "goldenrod") +
  geom_line(data = s.out[s.out$Ta == 3 & s.out$pct.rh == 100,],
            aes(x=Time/24, y = TBD.inf/24),
            size = 1, linetype = "solid") +
  geom_line(data = s.out[s.out$Ta == 6 & s.out$pct.rh == 100,],
            aes(x=Time/24, y = TBD.inf/24),
            size = 1, linetype = "dashed") +
  geom_line(data = s.out[s.out$Ta == 3 & s.out$pct.rh == 95,],
            aes(x=Time/24, y = TBD.inf/24),
            size = 1, linetype = "solid", color = "dodgerblue3") +
  geom_line(data = s.out[s.out$Ta == 6 & s.out$pct.rh == 95,],
            aes(x=Time/24, y = TBD.inf/24),
            size = 1, linetype = "dashed", color = "dodgerblue3") +
  # geom_line(data = s.out[s.out$Ta == 3 & s.out$pct.rh == 80,],
  #           aes(x=Time/24, y = TBD.inf/24),
  #           size = 1, linetype = "solid", color = "cadetblue2") +
  # geom_line(data = s.out[s.out$Ta == 6 & s.out$pct.rh == 80,],
  #           aes(x=Time/24, y = TBD.inf/24),
  #           size = 1, linetype = "dashed", color = "cadetblue2") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.text =element_text(size = 12, color = "black"),
        axis.title = element_text(size=14, color = "black",family="Times New Roman"),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Days of hibernation") +
  ylab("Torpor bout duration (days)") +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm"))) +
  ylim(0,20)



################################################################################
#### Predict TBD for plotting purposes                                      ####
################################################################################
temps <- seq(-5,20,1)
hd <- seq(20,100,1)

#Predict torpor bout duration based on continuous time and fungal growth
df.all <- data.frame()
for(s in unique(survival.figs$species)){
  s.params <- batLoad(bat.params, s)
  s.params$ttormax = 30*24
  s.params$TMRmin  = params$TMRmin[params$Species == s]
  s.params$Mass    = params$Mass[params$Species == s]
  s.params$pLean   = params$pLean[params$Species == s]
  s.params$pFat    = params$pFat[params$Species == s]
  s.params$rEWL.wing = params$rEWL.wing[params$Species == s]
  s.params$SA.wing   = params$SA.wing[params$Species == s]
  s.params$SA.body   = params$SA.body[params$Species == s]

  for(p in temps){
    for(h in hd){
      df = data.frame(Species = s, Ta = p, pct.rh = h, time = 0, tbd = 0)
      areaPd = fungalGrowth(Tb = p, fung.params = fung.params)*scaleFungalGrowth(pct.rh = h, fung.params = fung.params)
      time = torporTime(Ta = p, pct.rh = h, areaPd = areaPd, WNS = FALSE, bat.params = s.params, fung.params = fung.params)
      df = rbind(df, data.frame(Species = s, Ta =p, pct.rh = h, time = round(time), tbd = round(time)))
      t=3
      for(i in 1:1000){
        time = torporTime(Ta = p, pct.rh = h, areaPd = areaPd*df$time[t-2], WNS = TRUE, bat.params = s.params, fung.params = fung.params)
        df = rbind(df, data.frame(Species = s, Ta = p, pct.rh = h, time = df$time[t-1] + round(time), tbd = round(time)))
        t = t + 1
      }
      df.all = rbind(df.all, df)
    }
  }
}

#Remove 0 time step
df.all = df.all[df.all$time != 0,]
save(df.all, file =  "df_tbd.RData")

#Add to survival data frame
for(i in 2:dim(survival.figs)[1]){
  if(length(df.all$time[df.all$Ta == survival.figs$Ta[i] &
                        df.all$pct.rh == survival.figs$pct.rh[i] &
                        df.all$time >= survival.figs$time[i] &
                        df.all$Species == survival.figs$species[i]])==0){
    survival.figs$tbd[i] = survival.figs$tbd[i-1]
  } else{
    min.equal <- min(df.all$time[df.all$Ta == survival.figs$Ta[i] &
                                   df.all$pct.rh == survival.figs$pct.rh[i] &
                                   df.all$time >= survival.figs$time[i] &
                                   df.all$Species == survival.figs$species[i]])
    survival.figs$tbd[i] <- df.all$tbd[df.all$Ta == survival.figs$Ta[i] &
                                   df.all$pct.rh == survival.figs$pct.rh[i] &
                                   df.all$Species == survival.figs$species[i] &
                                   df.all$time == min.equal]
  }
}
save(survival.figs, file = "survResults_ALL_2May2019.RData")

#Plot
ggplot(survival.figs) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.border = element_rect( fill = NA)) +
  # geom_line(aes(x = survival.figs$time[survival.figs$Ta == 2 &
  #                                   survival.figs$pct.rh == 100 &
  #                                   survival.figs$time <= 180*24]/24,
  #               y = torporTime(Ta = 2, pct.rh = 100, areaPd = 0, WNS = 0,
  #                              bat.params = s.params, fung.params = fung.params)/24),
  #           color = "black", linetype = "solid", size = 1) +
  # geom_line(aes(x = survival.figs$time[survival.figs$Ta == 2 &
  #                                        survival.figs$pct.rh == 95 &
  #                                        survival.figs$time <= 180*24]/24,
  #               y = torporTime(Ta = 2, pct.rh = 95, areaPd = 0, WNS = 0,
  #                              bat.params = s.params, fung.params = fung.params)/24),
  #           color = "black", linetype = "dashed", size = 1) +
  geom_line(aes(x = survival.figs$time[survival.figs$Ta == 2 & survival.figs$species == "MYLU"&
                                         survival.figs$pct.rh == 100 &
                                         survival.figs$time <= 180*24]/24,
                y=survival.figs$tbd[survival.figs$Ta == 3 & survival.figs$species == "MYLU"&
                                      survival.figs$pct.rh == 100 &
                                      survival.figs$time <= 180*24]/24),
            color = "dodgerblue3", linetype = "solid", size = 1) +
  geom_line(aes(x = survival.figs$time[survival.figs$Ta == 2 & survival.figs$species == "MYLU"&
                                         survival.figs$pct.rh == 95 &
                                         survival.figs$time <= 180*24]/24,
                y=survival.figs$tbd[survival.figs$Ta == 2 & survival.figs$species == "MYLU"&
                                      survival.figs$pct.rh == 95 &
                                      survival.figs$time <= 180*24]/24),
            color = "dodgerblue3", linetype = "dashed", size = 1) +
  geom_line(aes(x = survival.figs$time[survival.figs$Ta == 6 & survival.figs$species == "MYLU"&
                                         survival.figs$pct.rh == 100 &
                                         survival.figs$time <= 180*24]/24,
                y=survival.figs$tbd[survival.figs$Ta == 6 & survival.figs$species == "MYLU"&
                                      survival.figs$pct.rh == 100 &
                                      survival.figs$time <= 180*24]/24),
            color = "gold", linetype = "solid", size = 1) +
  geom_line(aes(x = survival.figs$time[survival.figs$Ta == 6 & survival.figs$species == "MYLU"&
                                         survival.figs$pct.rh == 95 &
                                         survival.figs$time <= 180*24]/24,
                y=survival.figs$tbd[survival.figs$Ta == 6 & survival.figs$species == "MYLU"&
                                      survival.figs$pct.rh == 95 &
                                      survival.figs$time <= 180*24]/24),
            color = "gold", linetype = "dashed", size = 1) +
  theme(axis.title = element_text(size = 14,  family="serif"),
        axis.text = element_text(size = 14, color = "black", family="serif")) +
  xlab("Time (days)") +
  ylab("Predicted torpor bout duration (days)") +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm")) )

# OLD
# df.new <- data.frame()
# temps = seq(1,20,1)
# hd = seq(80, 100, 1)
# for(p in temps){
#   for(h in hd){
#     time <- seq(24, 24*360, 24)
#
#     for(i in time){
#       pd.area <- i*fungalGrowth(Tb = p, fung.params = fung.params)*scaleFungalGrowth(pct.rh = h, fung.params = fung.params)
#       tbd <- torporTime(Ta = p, pct.rh = h, areaPd = pd.area, WNS = TRUE,
#                         bat.params = s.params, fung.params = fung.params)
#       df.new <- rbind(df.new,
#                       data.frame(Ta = p,
#                                  pct.rh = h,
#                                  time = i,
#                                  pd.area = pd.area,
#                                  tbd = tbd))
#     }
#
#   }
# }

################################################################################
#### k constant of TBD                                                      ####
################################################################################
library(broom)

#Cut to just species of question and to temps above 0
species.tbd = s.out[s.out$Species == "MYLU",]
species.tbd = species.tbd[!is.na(species.tbd$Time),]

#Calculate k constant for each environment
temp = seq(0, 20, 1)
hd   = seq(60, 100, 1)
alpha <- data.frame()
for(t in temp){
  for(h in hd){
    df <- data.frame(t = species.tbd$Time[s.out$Ta == t & species.tbd$pct.rh == h],
                     y = species.tbd$TBD.inf[s.out$Ta == t & species.tbd$pct.rh == h])
    if(sd(df$y) == 0){
      alpha <- rbind(alpha, data.frame(Ta = t, pct.rh = h, yf = 0, y0 = 0, log.alpha = 0))
    } else{
      fit <- nls(y ~ SSasymp(t, yf, y0, log_alpha), data = df[3:361,])
      alpha <- rbind(alpha, data.frame(Ta = t, pct.rh = h, yf = coef(fit)[1], y0 = coef(fit)[2], log.alpha = coef(fit)[3]))


      # fit <- try(nls(y ~ SSasymp(t, yf, y0, log_alpha), data = df[3:361,]), silent = TRUE)
      # if(class(fit)=="nls"){
      #   alpha <- rbind(alpha, data.frame(Ta = t, pct.rh = h, yf = coef(fit)[1], y0 = coef(fit)[2], log.alpha = coef(fit)[3]))
      # } else{
      #   if(h>60){
      #     alpha <- rbind(alpha, data.frame(Ta = t, pct.rh = h,
      #                                      yf = alpha$yf[alpha$Ta == t & alpha$pct.rh == (h-1)],
      #                                      y0 =alpha$y0[alpha$Ta == t & alpha$pct.rh == (h-1)],
      #                                      log.alpha = alpha$log.alpha[alpha$Ta == t & alpha$pct.rh == (h-1)]))
      #   } else{
      #     alpha <- rbind(alpha, data.frame(Ta = t, pct.rh = h,
      #                                      yf = alpha$yf[alpha$Ta == (t-1) & alpha$pct.rh == (h)],
      #                                      y0 =alpha$y0[alpha$Ta == (t-1) & alpha$pct.rh == (h)],
      #                                      log.alpha = alpha$log.alpha[alpha$Ta == (t-1) & alpha$pct.rh == (h)]))
      #   }
      #
      # }
    }
  }
}

alpha = alpha[alpha$log.alpha != 0,]
ggplot(alpha[alpha$pct.rh >= 80 & alpha$Ta <= 15,], aes(x = Ta, y = log.alpha))+
  theme_bw() +
  geom_point(size = 2) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.text =element_text(size = 12, color = "black"),
        axis.title = element_text(size=14, color = "black",family="Times New Roman"),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab(expression(paste("Temperature (",degree,"C)", sep = ""))) +
  ylab("k constant") +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm")))

ggplot(alpha[alpha$pct.rh >= 80 & alpha$Ta <= 10,], aes(x = pct.rh, y = log.alpha))+
  theme_bw() +
  geom_point(size = 2) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.text =element_text(size = 12, color = "black"),
        axis.title = element_text(size=14, color = "black",family="Times New Roman"),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Relative humidity (%)") +
  ylab("k constant") +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm")))



qplot(t, y, data = augment(fit)) +
  geom_line(aes(y = .fitted))+
  geom_point(data = s.out[s.out$Ta == 7 & s.out$pct.rh == 96,] , aes(x=Time, y = TBD.inf), color = "red")



################################################################################
#### Validate torpor duration with Liam's data                              ####
################################################################################
setwd("C:/Users/Katie Haase/Desktop/Data/Validation Data/Skin Temp Data from Liam/")

#Read in all temperature data after it is checked and compiled
temp.data <- read.csv("offload_data.csv", header=TRUE)
temp.data$Date.Time <- as.POSIXct(strptime(temp.data$Date.Time, format = "%m/%d/%Y %H:%M"))
temp.data <- temp.data[temp.data$Phase != "Ignore",]

#Assign how the bat died
mass.data <- read.csv("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Input Files/Mass loss data for Katie.csv")
temp.data$death <- mass.data$Died.Euth[match(temp.data$iButtonID, mass.data$iButton)]

#Cut to cumulative temperature data (amount of time within each phase)
temp.times <- subset(temp.data, temp.data$Subset > 0 & temp.data$Phase != "Ignore")

#Assign first date to each file (in order to calculate Pd growth)
for(i in unique(temp.times$iButtonID)){
  temp.times$First.Date[temp.times$iButtonID == i] <- min(temp.data$Date.Time[temp.data$iButtonID == i])
}
temp.times$First.Date <- as.POSIXct(temp.times$First.Date, origin =  "1970-01-01")

#Calculate amount of time since beginning of measurements (for fungal growth estimation)
temp.times$diff.time <- as.numeric(difftime(temp.times$Date.Time, temp.times$First.Date , units = "hours"))

#Change time in phase to hours
temp.times$TimeinPhase = temp.times$Subset/60

#Subset to torpor only
tor.data <- temp.times[temp.times$Phase == "Torpor" & temp.times$TimeinPhase > 48 & temp.times$TimeinPhase < 400,]

#Read in bat parameter files
mylu.params <- batLoad(bat.params, species = "MYLU")
mylu.params$ttormax = 10*24
mylu.params$rEWL.wing=0.15
mylu.params$SA.wing = 25

#Calculate torpor bout duration
for(i in 1:nrow(tor.data)){
  mylu.params$Mass = tor.data$Mass[i]
  mylu.params$TMRmin =exp(-1.328 + (-0.059*log(tor.data$Mass[i])))
  tor.data$areaPd[i] = tor.data$diff.time[i]*fungalGrowth(Tb = 2, fung.params = fung.params, t.min = 0)*scaleFungalGrowth(pct.rh = 91.75+rnorm(1,mean=.5,.7), fung.params = fung.params)
  tor.data$TBD.EWL[i]   <- torporTime(Ta =tor.data$Value[i], pct.rh = 98, areaPd = tor.data$areaPd[i], WNS = ifelse(tor.data$Bat.Type[i] == "fungus", TRUE, FALSE), fung.params = fung.params, bat.params = mylu.params)
}

summary(tor.data[tor.data$Bat.Type == "control"& tor.data$death == "Euthanized",])
summary(tor.data[tor.data$Bat.Type == "fungus" & tor.data$death == "Euthanized"  & tor.data$TBD.EWL < 350,])

summary(lm(tor.data$TimeinPhase[tor.data$Bat.Type == "fungus"& tor.data$death == "Euthanized" & tor.data$TBD.EWL < 340] ~ tor.data$TBD.EWL[tor.data$Bat.Type == "fungus"& tor.data$death == "Euthanized" & tor.data$TBD.EWL < 340]))
save(tor.data, file="output_works.RData")
load("output_works.RData")
write.csv(tor.data, "tor_data.csv")
df = read.csv("tor_data.csv"); df = df[1:34,1:7]
summary(lm(df$TimeinPhase~df$TBD.EWL))

p.tbd <- ggplot(df, aes(x=TBD.EWL/24, y=TimeinPhase/24))+
  theme(panel.border = element_rect(fill = NA, color = "black", size=1.5)) +
  theme_bw() +
  geom_smooth(fill = "lightskyblue2", method='lm', color="black", level = 0.999)+
  geom_point(size=2)+
  geom_abline(slope=1, linetype=2, size = 1)+
  xlim(0,15)+
  ylim(0,15)+
  theme(axis.title.y = element_text(margin = margin(r=1)))+
  theme(axis.title.x = element_text(margin = margin(t=1)))+
  theme(axis.title = element_text(size = 14,  family="serif"),
        axis.text = element_text(size = 12, color = "black",  family="serif")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab(expression("Modeled Torpor Bout Duration (days)")) +
  ylab(expression("Measured Torpor Bout Duration (days)")) +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm")) )

  # scale_y_continuous(sec.axis = sec_axis(~ .*24, name = expression("Hours")))
plot(p.tbd)

################################################################################
#### Validate EWL model with new data                                       ####
################################################################################
#Read in data and subset to dry EWL data only
measured.EWL <- read.csv("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Input Files/mylu_wvp.csv")
measured.EWL <- measured.EWL[measured.EWL$Treatment == "dry" & is.na(measured.EWL$EWL) == FALSE  & measured.EWL$pct.rh<.20 & measured.EWL$pct.rh>0.01 & measured.EWL$Ta <10,]
measured.EWL <- measured.EWL[measured.EWL$batid != "MYLU28",]

#Determine species
species <- c("MYVE","PESU","COTO","EPFU","MYLU")

#Calculate EWL for each individual measurement
ewl.df <- data.frame()
for(s in species){
  s.params <- batLoad(bat.params, species = s)
  s.params$TMRmin = measured.EWL$TMRO2.g
  s.params$Mass = measured.EWL$Mass
  s.params$SA.body = measured.EWL$SA.body
  s.params$SA.wing = measured.EWL$SA.wing
  s.params$rEWL.body = measured.EWL$rEWL.body
  s.params$rEWL.wing = measured.EWL$rEWL.wing

  s.data <- measured.EWL[measured.EWL$Species == "lucifugus",]

  df <- data.frame(Species = species[s], ID = s.data$batid, Measured.EWL = s.data$EWL, ewl(Ta = s.data$Ta, pct.rh = s.data$pct.rh, t = 1, areaPd = 0, torpid = TRUE, WNS = FALSE, fung.params = fung.params, bat.params = s.params))

  ewl.df <- rbind(ewl.df, df)
}

#Compare measured and modeled EWL with a t-test to test significant difference
summary(lm(ewl$Measured.EWL~ewl$TotalEWL))

ewl = read.csv("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Input Files/ewl_plot.csv");ewl=ewl[1:63,1:5]
p.ewl <- ggplot(ewl, aes(x=TotalEWL, y=Measured.EWL))+
  theme(panel.border = element_rect( fill = NA, color = "black", size=1.5)) +
  theme_bw() +
  stat_smooth(fill = "lightskyblue2",method='lm', color="black", geom = "smooth",
              position = "identity", level=0.999999999)+
  geom_point(size=2)+
  geom_abline(slope=1, linetype=2, size = 1)+
  xlim(2,10)+
  ylim(2,10)+
  theme(axis.title.y = element_text(margin = margin(r=1)))+
  theme(axis.title.x = element_text(margin = margin(t=1)))+
  theme(axis.title = element_text(size = 14,  family="serif"),
        axis.text = element_text(size = 12, color = "black",  family="serif")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab(expression(paste("Modeled EWL (mg ",H[2],"O ",g^-1,")")))+
  ylab(expression(paste("Measured EWL (mg ",H[2],"O ",g^-1,")"))) +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm")) )

plot(p.ewl)

#Plot measured EWL data against wvp
ewl = read.csv("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Input Files/ewl_plot.csv");ewl=ewl[1:63,1:5]
ewl$dwvp <- rh2wvp(100, ewl$Ta) - rh2wvp(ewl$pct.rh*100, ewl$Ta)
p.raw1 <- ggplot(ewl[ewl$Measured.EWL < 10,], aes(x = ewl$Measured.EWL[ewl$Measured.EWL < 10], y = ewl$dwvp[ewl$Measured.EWL < 10]))+
  geom_point(size = 2, color = "black") +
  theme_bw() +
  xlim(2,10)+
  theme(axis.title = element_text(size = 14,  family="serif"),
        axis.text = element_text(size = 12, color = "black",  family="serif")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab(expression(paste("Water Vapor Pressure Deficit (kPa)"))) +
  ylab(expression(paste("Measured EWL (mg  ",H[2],"O ",g^-1,")"))) +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm")) )
summary(lm(ewl$Measured.EWL[ewl$Measured.EWL < 10]~ewl$dwvp[ewl$Measured.EWL < 10]))

p.raw2 <- ggplot(df, aes(x = areaPd, y = TimeinPhase))+
  geom_point(size = 2, color = "black") +
  theme_bw() +
  xlim(.1,.6)+
  theme(axis.title = element_text(size = 14,  family="serif"),
        axis.text = element_text(size = 12, color = "black",  family="serif")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab(expression(paste("Fungal Area (",cm^2,")"))) +
  ylab(expression(paste("Torpor Bout Duration (h)"))) +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm")) )

library(cowplot)
plot_grid(p.raw1,p.raw2, ncol = 1, align = "hv")
################################################################################
#### Validate survival model with Liam's/Jonasson's data                    ####
################################################################################
#Read in bat data
mylu.params = batLoad(bat.params, "MYLU")
mylu.params$ttormax = 480
mylu.params$SA.per = 0.50
mylu.params$SA.body = 39.2632638
mylu.params$SA.wing = 25.76848
mylu.params$rEWL.body = 0.11
mylu.params$rEWL.wing = 0.19
mylu.params$SA.plagio = 14.94572

#Read in mass data
mass.data <- read.csv("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Input Files/Mass loss data for Katie.csv")
mass.data <- mass.data[!is.na(mass.data$Begin.Mass),]

#Calculate actual weight loss
mass.data$Weight.Loss <- mass.data$Begin.Mass - mass.data$End.Mass

#Calculate fat loss for each bat
# for(i in 1:dim(mass.data)[1]){
#   hib.length <- abs(as.numeric(difftime(as.Date("2013-11-30"), as.Date.character(as.character(mass.data$EndDate[i]), format = "%m/%d/%Y"), units = "days")))
#   env.df  <- buildEnv(temp = c(7,7), pct.rh = c(98,98), range.res.temp = 1, range.res.rh = 1, twinter = ceiling(hib.length/30), winter.res = 24)
#   de.df <- data.frame(hibernationModel(env = env.df, bat.params = mylu.params, fung.params = fung.params))
#   mass.data$FatCon[i] <- ifelse(mass.data$Treatment[i] == "control", de.df$n.g.fat.consumed[hib.length],de.df$g.fat.consumed[hib.length])
# }

#Read in phase data
phase <- read.csv("C:/Users/Katie Haase/Desktop/Data/Skin Temp Data from Liam/Phase.csv")

#Read in arousal dates
dates <- read.csv("C:/Users/Katie Haase/Desktop/Data/Skin Temp Data from Liam/arousaldates.csv")

#Calculate fat loss with observed torpor time
for(i in 2:dim(dates)[2]){
  #Subset to column of dates
  d.data <- dates[!is.na(dates[,i]),c(1,i)]

  #Define iButton id
  id <- colnames(d.data)[2]

  #Assign parameters
  i.params = s.params
  i.params$TMRmin = ifelse(mass.data$Treatment[mass.data$iButton == id] == "control",0.58,0.7)
  i.params$Mass = mass.data$Begin.Mass[mass.data$iButton == id]

  #Determine average arousal, cooling, and euthermic time for bat
  arousal <- mean(phase$TimeinPhase[phase$iButtonID == id & phase$Phase.NoCA == "Arousal"])
  cooling <- mean(phase$TimeinPhase[phase$iButtonID == id & phase$Phase.NoCA == "Cooling"])
  euthermia <- 3

  #Determine total time in torpor
  d.data$Date <- as.Date.character(as.character(d.data$Date), format = "%m/%d/%Y")
  torpor <- sum(c(0,difftime(d.data$Date[2:length(d.data$Date)], d.data$Date[1:(length(d.data$Date)-1)], units = "hours")))

  #Subtract arousal, cooling, and euthermia time per torpor bout
  torpor <- torpor - ((arousal + cooling + euthermia)*length(d.data$Date))

  #Calculate total energy based on torpor, arousal, cooling, and euthermia time
  arousal.E <- arousalEnergy(Ta = 7, bat.params = i.params)*arousal*length(d.data$Date)
  cooling.E <- coolEnergy(Ta = 7, bat.params = i.params)*cooling*length(d.data$Date)
  euthermia.E <- euthermicEnergy(Ta = 7, bat.params = i.params)*euthermia*length(d.data$Date)
  torpor.E <- torporEnergy(Ta = 7, WNS = ifelse(mass.data$Treatment[mass.data$iButton == id] == "control",FALSE,TRUE), bat.params = i.params, q = calcQ(7))*torpor

  #Sum all energy and convert to fat loss
  mass.data$FatConsumed[mass.data$iButton == id] <- ((arousal.E + cooling.E + euthermia.E + torpor.E)*19.7)/(37.1*1000)

}

mass.data = mass.data[mass.data$Died.Euth == "Euthanized",]
load("C:/Users/Katie Haase/Desktop/Data/Validation Data/Skin Temp Data from Liam/output_works.RData")
summary(lm(mass.data$Weight.Loss ~ mass.data$FatConsumed))
plot(mass.data$FatConsumed, mass.data$Weight.Loss, xlim = c(0,4), ylim = c(0,4))


#Jonasson Validation
data <- read.csv("C:/Users/Katie Haase/Desktop/Data/Validation Data/JonassonValidation.csv")
data$diff <- abs(as.numeric(difftime(as.Date.character(as.character(data$StartDate), format = "%m/%d/%Y"), as.Date.character(as.character(data$EndDate),format = "%m/%d/%Y"))))
env.df  <- buildEnv(temp = c(6), pct.rh = c(85,85), range.res.temp = 1, range.res.rh = 1, twinter = 2, winter.res = 24)
mylu.params <- batLoad(bat.params, "MYLU")
for(i in 1:12){
  i.params = mylu.params
  i.params$mass = data$StartMass[i]
  surv <- hibernationModel(env = env.df, bat.params = i.params, fung.params = fung.params)
  data$fat[i] <- surv$n.g.fat.consumed[length(surv$time[surv$surv.inf == 1])]
}
data$WeightLoss <- data$StartMass - data$EndMass

t.test(data$WeightLoss, data$fat)

################################################################################
#### Validate survival model with swarming/emergence data                   ####
################################################################################
validate <- read.csv("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Input Files/MassValidation.csv")
validate <- validate[validate$Useable == 1,]

#Calculate mass loss
validate$mass.diff <- ifelse(validate$pittag == validate$pittag[-1], validate$Mass-validate$Mass[-1],0)

#Calculate time difference
validate$date <- as.Date.character(as.character(validate$date), format = "%d-%b-%Y")
validate$time.diff <- abs(ifelse(validate$pittag == validate$pittag[-1], difftime(validate$date, validate$date[-1], units = "hours"),0))
validate = validate[validate$time.diff <= 8600,]
validate = validate[validate$Bat.Activity != "Hibernating",]

#Subset to start date
validate <- validate[validate$mass.diff >=2,]
validate <- validate[!is.na(validate$mass.diff),]

#Cut to just MYLU
validate <- validate[validate$species == "MYLU",]

#Read in and assign bat parameters
mylu.params <- batLoad(bat.params, "MYLU")

#Run through each site
validate.df <- data.frame()
for(s in unique(validate$location)){
  #Subset to site
  site <- validate[validate$location == s,]

  for(i in 1:dim(site)[1]){
    if(s == "Microwave"){ #bloceki
      Ta = rnorm(1,2,.2)
      pct = rnorm(1,96,1)
    } else if(s == "Firecamp"){
      Ta =   rnorm(1,3.55,0.51)
      pct = rnorm(1,86.33,3.14)
    } else if(s == "Squeaky"){ #McKillop
      Ta = rnorm(1,6.2, .51)
      pct = rnorm(1,83,1.5)
    } else if(s == "Abyss") { #Klug-Baerwald
      Ta = rnorm(1, 8.6, .2)
      pct = rnorm(1,80,2.5)
    } else if(s == "St.George"){#Czenze
      Ta = rnorm(1,4.2,0.5)
      pct = rnorm(1,75,1.5)
    } else{
      Ta = rnorm(1,8.9,.2)
      pct = rnorm(1,88,1)
    }

    #Build environment with mean data
    env.df  <- buildEnv(temp = c(Ta,Ta), pct.rh = c(pct,pct), range.res.temp = 1, range.res.rh = 1, twinter = 12, winter.res = 1)

    #Add individual characteristics
    mylu.params$Mass <- site$Mass[i]
    mylu.params$TMRmin =exp(-1.328 + (-0.059*log(site$Mass[i])))
    mylu.params$SA.body <- 10*(site$Mass[i]^0.67)
    mylu.params$pLean <- rnorm(1,0.53, 0.2)

    df <- data.frame(hibernationModel(env = env.df, bat.params = mylu.params, fung.params = fung.params))
    time.df <- df[df$time == site$time.diff[i],]
    site$Fat.Loss[i] <- time.df$n.g.fat.consumed
  }
  validate.df = rbind(validate.df,site)
}

#Compare values
summary(lm(validate.df$mass.diff ~ validate.df$Fat.Loss))
plot(validate.df$mass.diff ~ validate.df$Fat.Loss)


save(validate.df, file = "fat_val.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Input Files/fat_val.RData")

fat = validate.df
p.surv <- ggplot(fat, aes(x=Fat.Loss, y=mass.diff))+
  theme(panel.border = element_rect( fill = NA, color = "black", size=1.5)) +
  theme_bw() +
  geom_smooth(fill = "lightskyblue2", method='lm', color="black", level=0.9999, formula = y ~ x)+
  geom_point(size=2)+
  geom_abline(slope=1, linetype=2, size = 1)+
  xlim(2,8)+
  ylim(2,8)+
  theme(axis.title.y = element_text(margin = margin(r=1)))+
  theme(axis.title.x = element_text(margin = margin(t=1)))+
  theme(axis.title = element_text(size = 14,  family="serif"),
        axis.text = element_text(size = 12, color = "black",  family="serif")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab(expression("Modeled Fat Loss (g)"))+
  ylab(expression("Measured Mass Loss (g)")) +
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0,0,0), "cm")),
        axis.text.y = element_text(margin=unit(c(0,0.3,0,0), "cm")))
plot(p.surv)
summary(lm(fat$mass.diff ~ fat$Fat.Loss))



library(cowplot)
plot_grid(p.ewl, p.tbd, p.surv, ncol = 1, align = "hv")

################################################################################
#### Plot histogram of parameters for manuscript                            ####
################################################################################
params <- read.csv("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Input Files/params4hist.csv")

median(params$Mean[params$Value == "teu" & params$Species == "MYLU"])
median(params$Max[params$Value == "ttormax" & params$Species == "MYLU"], na.rm = T)

hist(params$Max[params$Value == "ttormax" & params$Species == "MYLU"])

df <- params[params$Value == "ttormax" & params$Species == "MYLU",]
ggplot(df, aes(x=Max)) +
  geom_histogram(binwidth=200, color="black", fill="white") +
  theme(panel.border = element_rect( fill = NA, color = "black")) +
  ylab("Count") +
  xlab(expression(paste("Maximum ",t[tor], " (h)"))) +
  theme(axis.title = element_text(size = 14),#  family="serif"),
        axis.text = element_text(size = 14, color = "black")) +  # family="serif"),
  theme(strip.text.x = element_text(size = 13)) +
  theme(axis.ticks = element_line(color = "black"))


################################################################################
#### Run model to include flying for supp materials                         ####
################################################################################
env.df  <- buildEnv(temp = c(5,5), pct.rh = c(98,98), range.res.temp = 1, range.res.rh = 1, twinter = 12, winter.res = 24)
s.params <- batLoad(bat.params, "MYLU")
s.params$ttormax = 1300

#Calculate dynamic energy over range of environmental conditions
df.flight = data.frame()
for(f in seq(0,1,0.05)){
  s.params$pFly = f
  de.df <- data.frame(hibernationModel(env = env.df, bat.params = s.params, fung.params = fung.params))
  days.null = max(de.df$time[de.df$surv.null == 1])/24
  days.inf  = max(de.df$time[de.df$surv.inf == 1])/24
  df.flight = rbind(df.flight, data.frame(PropFly = f, Healthy = days.null, WNS = days.inf))
}

plot(df.flight$PropFly, df.flight$Healthy, col = "white", ylim = c(50,400),
     xlab = "Proportion of Euthermia Spent Flying",
     ylab = "Days until Fat Exhaustion")
lines(df.flight$PropFly, df.flight$Healthy, col = "blue", lwd =2)
lines(df.flight$PropFly, df.flight$WNS, lwd = 2)

ggplot(data = df.flight, aes(x=PropFly, y = Healthy))+
  theme_classic() +
  geom_smooth(aes(y=Healthy), color = "blue", se=FALSE) +
  geom_smooth(aes(y=WNS), se=FALSE, color = "black" )+
  theme(panel.border = element_rect( fill = NA)) +
  xlab("Proportion of Euthermia Spent Flying")+
  ylab("Days until Fat Exhaustion") +
  theme(axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 14, color = "black"))










#### PRCC ####
prcc = function( par.mat, model.output, routine = "blower",
                 par.names = NA,output.names = NA, ...){

  #Make sure par.mat and model.output are matrices
  par.mat = as.matrix(par.mat)
  model.output = as.matrix(model.output)

  #How many parameter sets are there?
  n = length(par.mat[,1])

  #How many parameters are there?
  p = length(par.mat[1,])

  #How many model outputs are we calculating PRCCs for?
  k = length(model.output[1,])

  #Find the ranks of the parameter values and the model output
  par.rank = apply(par.mat,2,rank)#,...)
  output.rank = apply(model.output,2,rank)#,...)

  #What is the average rank?
  ave.rank = (1 + n)/2

  #Create a list object to store the PRCCs
  results = list()

  results$num.sets = n

  #Try to automatically get parameter and output names if they are not
  #given
  if( sum(is.na(par.names)) > 0){par.names=dimnames(par.mat)[[2]]}
  if( sum(is.na(output.names)) > 0){output.names=dimnames(model.output)[[2]]}

  ########################################################################
  #Calculate the PRCCs using Appendix A from Blower and Dowlatabadi (1994)
  ########################################################################
  if( routine == "blower" ){

    #Do the calculation for each model output
    for( i in 1:k ){

      work.mat = cbind(par.rank,output.rank[,i])

      C.temp = matrix(0,nrow=p+1,ncol=p+1)

      #Calculate the C matrix
      for( j in 1:(p+1) ){
        for( l in 1:(p+1) ){

          C.temp[j,l]=sum((work.mat[,j]-ave.rank)*(work.mat[,l]-ave.rank))/
            sqrt(sum((work.mat[,j]-ave.rank)^2)*
                   sum((work.mat[,l]-ave.rank)^2))
        }
      }

      #Calculate the B matrix (qr helps with inversion)
      B.temp = solve(qr(C.temp))

      coeff.val = rep(0,p)

      #Calculate the PRCC
      for( j in 1:p ){

        coeff.val[j] = -B.temp[j,p+1]/sqrt(B.temp[j,j]*B.temp[p+1,p+1])

      }

      #Calculate the t-test statistics and p-values
      t.val = coeff.val*sqrt((n-2)/1-coeff.val)
      p.val = 2*pt(abs(t.val),df=(n-2),lower.tail=F)

      #Output the results
      results[[output.names[i]]] = data.frame(
        prcc = coeff.val,
        t.value = t.val,
        p.value = p.val,
        row.names = par.names)
    }

    return(results)
  }

  ########################################################################
  #Calculate the PRCCs using regression methods
  ########################################################################
  else if( routine == "regression" ){

    #Do the calculation for each model output
    for( i in 1:k ){

      coeff.val = rep(0,p)

      #Calculate the PRCC
      for( j in 1:p ){

        #Variation in output that can not be explained by all other predictors
        #(except the predictor of interest)
        fit.y = lm(output.rank[,i] ~ par.rank[,-j])

        #Variation in the predictor of interest that can not be explained
        #by the other predictors
        fit.x = lm(par.rank[,j] ~ par.rank[,-j])

        #PRCC is the correlation between the residuals of the two
        #regressions above
        coeff.val[j] = cor(fit.y$residuals,fit.x$residuals)

      }

      #Calculate the t-test statistics and p-values
      t.val = coeff.val*sqrt((n-2)/1-coeff.val)
      p.val = 2*pt(abs(t.val),df=(n-2),lower.tail=F)

      #Output the results
      results[[output.names[i]]] = data.frame(
        prcc = coeff.val,
        t.value = t.val,
        p.value = p.val,
        row.names = par.names)
    }

    return(results)
  }

  else{ return("Error: Calculation type is invalid. Must be either 'blower' or 'regression'") }

}

#### WVP ####
rh2wvp <- function(pct.rh, Ta){
  wvp = (pct.rh*0.01)*(0.61 * exp((17.50 * Ta)/(Ta + 240.97)))
  return(wvp)
}

#### flying functions ####
batDynamic <- function(t,y, params){
  with(c(as.list(y),params),{
    # time in torpor
    ttor <- torporTime(Ta = Ta, pct.rh = pct.rh, areaPd = FungalArea, WNS = WNS,
                       bat.params = params, fung.param = params)
    # energy cost for torpor
    Etor = torporEnergy(Ta = Ta, WNS = WNS, areaPd = FungalArea,
                        bat.params = params)
    # time to arouse and cool
    tar <- arousalTime(Ta = Ta, bat.params = params)
    tc <- coolTime(Ta = Ta, bat.params = params)
    tfl <- flyingTime(bat.params = params)
    teu <- euthermicTime(bat.params = params) - flyingTime(bat.params = params)
    # change in TorporProp (pT)/dt
    dpTdt <- (pE/teu + pAr/tar + pC/tc + pFl/tfl)/4 - pT/ttor
    # change in ArousalProp (pAr)/dt
    dpAdt <- (pE/teu + pC/tc + pT/ttor + pFl/tfl)/4 - pAr/tar
    # change in CoolProp (pC)/dt
    dpCdt <- (pE/teu + pAr/tar + pT/ttor + pFl/tfl)/4 - pC/tc
    # change in EuthermicProp (pE)/dt
    dpEdt <- (pT/ttor + pAr/tar + pC/tc + pFl/tfl)/4 - pE/teu
    # Cange in FlyingProp (pFl)/dt
    dpFldt <- (pT/ttor + pAr/tar + pC/tc + pE/teu)/4 - pFl/tfl
    # change in EnergyConsumed/dt
    dJdt  <- Eeu*pE + Etor*pT + Ear*pAr + Ec*pC + Efl*pFl
    # change in precEArousal/dt
    dpJdt <- Eeu*pE +Ear*pAr + Ec*pC + Efl*pFl
    #change in FungalArea/dt
    dFdt  <- growth*pT

    list(c(dpTdt, dpAdt, dpCdt, dpEdt,dpFldt, dJdt, dpJdt, dFdt))
  })
}

hibernationModel <- function(env, bat.params, fung.params){
  out <- list()
  mod.params <- as.list(c(bat.params, fung.params))
  with(mod.params,{
    #Mechanism to do WNS + and - in one function
    for(i in 1:2){
      ifelse(i == 1, inf <- T, inf <- F)
      # apply model engine across env dataframe
      results <- apply(env[[1]], 1,function(x){
        Ta <- x[[1]]
        pct.rh <- x[[2]]
        if(beta3 >= Teu){
          warning("The model assumes fungal growth does not occure at euthermic
                  temperature. \n This assumption is violated in the current
                  parameter range")
        }
        # determinine Ttorpid @ Ta
        Ttor <- ifelse(Ta > Ttormin, Ta, Ttormin)
        # determine Tb
        Tb <- ifelse(Ttor < Teu, Ttor, Teu)
        # create values that will be fed into the dynamic model
        values <- c(Tb = Tb, Ta = Ta, Ttor = Ttor, WNS = inf, pct.rh= pct.rh,
                    # Fungal growth area
                    growth = fungalGrowth(Tb = Ta, fung.params = mod.params)*
                      scaleFungalGrowth(pct.rh = pct.rh, fung.params = mod.params),
                    # Energy cost for euthermia
                    Eeu = euthermicEnergy(Ta = Ta, bat.params = mod.params),
                    # Energy costs for flying during euthermia
                    Efl = flyingEnergy(Ta = Ta, bat.params = mod.params),
                    # Energy cost for arousal from torpor
                    Ear = arousalEnergy(Ta = Ta,  bat.params = mod.params),
                    # Energy cost for cooling from euthermic
                    Ec = coolEnergy(Ta = Ta, bat.params = mod.params),
                    mod.params)
        # Call differential equation model
        det.results <- data.table(lsoda(y = c(pT = 1, # Inital values
                                              pAr = 0,
                                              pC = 0,
                                              pE = 0,
                                              pFl = 0,
                                              EnergyConsumed = 0,
                                              EnergyBoutArousal = 0,
                                              FungalArea = 0),
                                        # Time to solve across
                                        times = env[[2]],
                                        func = batDynamic,
                                        parms = values))
        # Helper function for energy calculations
        # This is needed because the dif eqs solve for the change in (x)
        # per time, not (x) @ t
        MaxToCurrent <- function(x){
          cummax(x)[-1]
        }
        # Energy costs for up to that point in the winter
        e.winter <- MaxToCurrent(det.results$EnergyConsumed)
        ar.winter <- MaxToCurrent(det.results$EnergyBoutArousal)
        # Convert units to grams of fat
        fat.consumed <- kcal.to.g(e.winter)
        ar.fat <- kcal.to.g(ar.winter)
        # What precent of costs are due to arousals
        prec.ar <- ar.fat/fat.consumed
        # Proportion of time in torpor
        prop.tor <- MaxToCurrent(det.results$pT)
        prop.ar <- MaxToCurrent(det.results$pAr)
        prop.fl <- MaxToCurrent(det.results$pFl)
        Tb <- Tb
        # Creat dataframe of results for intermediate product
        results <- data.table(Ta = rep(Ta,length(env[2])),
                              pct.rh = rep(pct.rh,length(env[2])),
                              cbind(g.fat.consumed = c(0,fat.consumed),
                                    pEnergyBoutArousal = c(0, prec.ar),
                                    Pd.growth = c(0,
                                                  MaxToCurrent(det.results$FungalArea)),
                                    time = det.results$time,
                                    Prop.tor = c(1,prop.tor),
                                    Prop.Ar = c(0,prop.ar),
                                    Prop.Fl = c(0,prop.fl),
                                    Tb = Tb))
        return(results)
        })
      foo <- rbindlist(results)
      out[[i]] <- foo
      }
    # Create one better dataframe with all pertinent columns
    out.dt <- cbind(out[[1]], n.g.fat.consumed = out[[2]]$g.fat.consumed,
                    n.pEnergyBoutArousal = out[[2]]$pEnergyBoutArousal,
                    n.Prop.tor = out[[2]]$Prop.tor,
                    n.Prop.Ar = out[[2]]$Prop.Ar)

    out.fin <- out.dt %>%
      # Create columns with survival outcomes  based on avaliable fat reserves
      mutate(sub.fat = kcal.to.g(arousalEnergy(Ta=Ta, bat.params = mod.params) +
                                   (24*euthermicEnergy(Ta=Ta, bat.params=mod.params)))) %>%
      mutate(surv.inf  = ifelse((Mass*pFat) >= g.fat.consumed+sub.fat,1,0)) %>%
      mutate(surv.null = ifelse((Mass*pFat) >= n.g.fat.consumed+sub.fat,1,0))
    return(data.table(out.fin))
  })
}

#### multiplot ####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



data <- read.csv("MTandCanada.csv")

data$DateTIme <- as.POSIXct(as.character(data$DateTIme), format = "%m/%d/%Y")
data$Month <- strftime(data$DateTIme, format = "%m")
data$Day <- strftime(data$DateTIme, format = "%d")
data$Year <- ifelse(data$Month >= 10, "2009", "2010")
data$Date <- as.Date(with(data, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
data$dt <- as.POSIXct(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")
data <- data[data$Month !="06", ]
ggplot(data, aes(x = dt, y = Ta, color = Site)) +
  theme_bw() +
  geom_point() +
  xlab("") +
  ylab(expression(paste("Temperature (C)")))


