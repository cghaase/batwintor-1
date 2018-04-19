library(batwintor)
library(grofit)
library(ggplot2)
library(testthat)
library(deSolve)
library(data.table)
library(dplyr)
library(beepr)
library(devtools)
library(gridExtra)
#devtools::document()

#Read in bat data
data("bat.params")
bat.params$pFly = 0.00000001
bat.params$Ct = 0.2
bat.params$SA.wing <- bat.params$SA.wing*2
bat.params$SA.plagio <- bat.params$SA.plagio*2
bat.params$rEWL = bat.params$rEWL*(10*(bat.params$Mass^0.67))/bat.params$SA.wing

#Load fungal growth
fung.params <- fungalSelect("Chaturvedi")

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
hypercube=randomLHS(n=nspaces, k=29)

#Create range of parameters (k has to equal number listed)
mins = with(c(as.list(mylu.params),as.list(fung.params)),
            c(   			    ## set mins for each parameters-exclude those not to be varied if any
              mass = 0.9*7.8,
              RMR = 0.9*2.6,
              TMRmin = 0.9*0.0153,
              Teu = 0.9*37,
              Tlc = 0.9*34,
              Ttormin = 0.9*2,
              Ceu = 0.9*0.2638,
              Ct = 0.9*0.055,
              S = 0.9*0.1728,
              ttormax = 0.9*792,
              teu = 0.9*3,
              WR = 0.9*48,
              CR = 0.9*32.16,
              rEWL = 0.9*0.1826,
              mrPd = 0.9*1.4,
              aPd = 0.9*0.21,
              rPd = 0.9*1.525,
              pMass.i = 0.9*0.027,
              pMass = 0.9*0.027,
              pFly = 0.0001,
              pLean = 0.9*0.532,
              pFat = 0.9*0.216,
              SA.wing = 0.9*74.8,
              SA.plagio = 0.9*38.72,
              beta1 = 0.9*0.0007751467,
              beta2 = 0.9*0.2699683,
              beta3 = 0.9*19.7309,
              mu1 = 0.9*0.000150958,
              mu2 = 0.9*-0.009924594
        ))

maxs = with(c(as.list(mylu.params),as.list(fung.params)),
            c( 				    ## set maxs for each parameters-exclude those not to be varied if any
              mass = 1.1*7.8,
              RMR = 1.1*2.6,
              TMRmin = 1.1*0.0153,
              Teu = 1.1*37,
              Tlc = 1.1*34,
              Ttormin = 1.1*2,
              Ceu = 1.1*0.2638,
              Ct = 1.1*0.055,
              S = 1.1*0.1728,
              ttormax = 1.1*792,
              teu = 1.1*3,
              WR = 1.1*48,
              CR = 1.1*32.16,
              rEWL = 1.1*0.1826,
              mrPd = 1.1*1.4,
              aPd = 1.1*0.21,
              rPd = 1.1*1.525,
              pMass.i = 1.1*0.027,
              pMass = 1.1*0.027,
              pFly = .0001,
              pLean = 1.1*0.532,
              pFat = 1.1*0.216,
              SA.wing = 1.1*74.8,
              SA.plagio = 1.1*38.72,
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
                              "S",
                              "ttormax",
                              "teu",
                              "WR",
                              "CR",
                              "rEWL",
                              "mrPd",
                              "aPd",
                              "rPd",
                              "pMass.i",
                              "pMass",
                              "pFly",
                              "pLean",
                              "pFat",
                              "SA.wing",
                              "SA.plagio",
                              "beta1",
                              "beta2",
                              "beta3",
                              "mu1",
                              "mu2"
          )

paramset<-hypercubeadj

#Determine environment to run model over
env.df  <- buildEnv(temp = c(2,20), pct.rh = c(60,100), range.res.temp = 5, range.res.rh = 5, twinter = 10, winter.res = 1)

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

    #Calculate EWL over an hour
    res.b = ewl(Ta=paramset.H$env[j,1], pct.rh=paramset.H$env[j,2], areaPd = area*24,  t=24, fung.params = as.list(paramset[i,]), bat.params = as.list(paramset[i,]), torpid = TRUE, WNS = TRUE)$TotalEWL

    #Calculate torpor duration at end of winter (time for Pd growth is max survival time from res.a)
    res.c = torporTime(Ta=paramset.H$env[j,1], pct.rh=paramset.H$env[j,2], WNS = TRUE, fung.params = as.list(paramset[i,]), bat.params = as.list(paramset[i,]), areaPd = area*res.a)

    #Fill in results matrix
    res_out.a[j,] <- res.a
    res_out.b[j,] <- res.b
    res_out.c[j,] <- res.c
  }

  sen.results.a[i,]<-as.vector(t(res_out.a))
  sen.results.b[i,]<-as.vector(t(res_out.b))
  sen.results.c[i,]<-as.vector(t(res_out.c))
}
save(sen.results.a, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_surv_20March2018.RData")
save(sen.results.b, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_ewl_20March2018.RData")
save(sen.results.c, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_tor_20March2018.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_surv_20March2018.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_ewl_20March2018.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_tor_20March2018.RData")

#Apply function to new model results [removed pFly because it was throwing an error and we don't care about it]
PRCCresults.a <- prcc(par.mat = paramset[,-20], model.output = sen.results.a[,-20], routine = "blower", par.names = colnames(paramset[,-20]),output.names = seq(1,ncol(sen.results.a[,-20])))
PRCCresults.b <- prcc(par.mat = paramset[,-20], model.output = sen.results.b[,-20], routine = "blower", par.names = colnames(paramset[,-20]),output.names = seq(1,ncol(sen.results.b[,-20])))
PRCCresults.c <- prcc(par.mat = paramset[,-20], model.output = sen.results.c[,-20], routine = "blower", par.names = colnames(paramset[,-20]),output.names = seq(1,ncol(sen.results.c[,-20])))

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
means.a <- data.frame(means=PRCCresults.a[[1]][c(1:8,10:18,20:25,27:28),1], label = row.names(PRCCresults.a[[1]][c(1:8,10:18,20:25,27:28),]))                             #survival
#means.b <- data.frame(means=PRCCresults.b[[1]][c(1,3,14:17,22:28),1], label = row.names(PRCCresults.a[[1]][c(1,3,14:17,22:28),]))         #ewl
means.b <- data.frame(means=PRCCresults.b[[1]][c(1,3,6,14:18,20,22:25,27,28),1],label = row.names(PRCCresults.b[[1]][c(1,3,6,14:18,20,22:25,27,28),]))     #tbd
means.b[c(3,8,9),1] <- 0 #putting to 0 for plotting purposes since these parameters aren't used int he EWL model
means.c <- data.frame(means=PRCCresults.c[[1]][c(1,3,6,14:18,20,22:25,27,28),1],label = row.names(PRCCresults.a[[1]][c(1,3,6,14:18,20,22:25,27,28),]))     #tbd

means.all <- data.frame(rbind(means.b, means.c), Value = c(rep("EWL",length(means.c$means)), rep("TBD",length(means.c$means))))
#Plot mean with significance
# install.packages("extrafont")
# library(extrafont)
# font_import()
# loadfonts(device="win")       #Register fonts for Windows bitmap output
# fonts()
# windowsFonts(A = windowsFont("Times New Roman"))
# op <- par(family = "serif")
names.a = expression("Body Mass","RMR","TMR"[min],"T"[eu],"T"[lc],"T"[tormin],"C"[eu],"C"[tor],"t"[tormax],"t"[eu],"Warming Rate","Cooling Rate","Rate of EWL",
          "TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd","EWL Threshold","% Body Mass as Lean","% Body Mass as Fat",
          "Wing Surface Area", "Plagiopatagium Surface Area",beta[1],beta[2],mu[1],mu[2])
names.b = expression("Body Mass","TMR"[min],"Rate of EWL","TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd",
                     "Wing Surface Area", "Plagiopatagium Surface Area", beta[1],beta[2],mu[1],mu[2])
names.c = expression("Body Mass","TMR"[min],"T"[tormin],"Rate of EWL","TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd","EWL Threshold","% Body Mass as Lean",
                     "Wing Surface Area", "Plagiopatagium Surface Area",beta[1],beta[2],mu[1],mu[2])

t.cutoff=qt(0.05/2,df=100-2)
sig.cutoffs=c( (-(t.cutoff^2)-sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)),
               (-(t.cutoff^2)+sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)))

ggplot(means.a, aes(y =means.a[,1], x=label))+
  geom_bar(stat='identity',aes(y = means.a[,1], x =label), alpha=.55) +
  coord_flip() +
  geom_hline(yintercept = sig.cutoffs,linetype = 2, size=1.05) +
  geom_hline(yintercept = 0) +
  theme(panel.border = element_rect( fill = NA)) +
  theme(axis.text = element_text(size=14, color = "black"), axis.title=element_text(size=14)) +
  # theme(text=element_text(family="A")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #scale_x_discrete(labels = names.a) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  labs(y = "PRCC") + labs(x = "") + ylim(-1,1)

ggplot(means.b, aes(y =means.b[,1], x=label))+
  geom_bar(stat='identity',aes(y = means.b[,1], x =label), alpha=.55) +
  coord_flip() +
  geom_hline(yintercept = sig.cutoffs,linetype = 2, size=1.05) +
  geom_hline(yintercept = 0) +
  theme(panel.border = element_rect( fill = NA)) +
  theme(axis.text = element_text(size=14, color = "black"), axis.title=element_text(size=14)) +
  #theme(text=element_text(family="A")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  scale_x_discrete(limits = means.b$label, labels = names.b) +
  labs(y = "PRCC") + labs(x = "") + ylim(-1,1)

ggplot(means.c, aes(y =means.c[,1], x=label))+
  geom_bar(stat='identity',aes(y = means.c[,1], x =label), alpha=.55) +
  coord_flip() +
  geom_hline(yintercept = sig.cutoffs,linetype = 2, size=1.05) +
  geom_hline(yintercept = 0) +
  theme(panel.border = element_rect( fill = NA)) +
  theme(axis.text = element_text(size=14, color = "black"), axis.title=element_text(size=14)) +
  # theme(text=element_text(family="A")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_discrete(limits = means.c$label, labels = names.c) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  labs(y = "PRCC") + labs(x = "") + ylim(-1,1)

ggplot(means.all, aes(y =means.all[,1], x=label))+
  theme_bw() +
  facet_wrap(~Value, ncol = 2) +
  geom_bar(stat='identity',aes(y = means.all[,1], x =label), alpha=.55) +
  coord_flip() +
  geom_hline(yintercept = sig.cutoffs,linetype = 2, size=1.05) +
  geom_hline(yintercept = 0) +
  theme(panel.border = element_rect( fill = NA)) +
  theme(axis.text = element_text(size=14, color = "black"), axis.title=element_text(size=14)) +
  # theme(text=element_text(family="A")) +
  theme(strip.text.x = element_text(size = 13)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_discrete(limits = means.c$label, labels = names.c) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  labs(y = "PRCC") + labs(x = "") + ylim(-1,1)



# barplot(means.a[,1], xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
#         names.arg= names.a, horiz=TRUE, col = ifelse(means.a > sig.cutoffs[1] & means.a < sig.cutoffs[2], "grey85", "steelblue3"), main = "Survival")
# abline(v=sig.cutoffs,lty=2,col="red")
# abline(v=0,lty=1,col="black")
#
# barplot(means.b[,1], xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
#         names.arg= names.b, horiz=TRUE, col = ifelse(means.b > sig.cutoffs[1] & means.b < sig.cutoffs[2], "grey85", "steelblue3"), main = "Total Evaporative Water Loss")
# abline(v=sig.cutoffs,lty=2,col="red")
# abline(v=0,lty=1,col="black")
#
# barplot(means.c[,1], xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
#         names.arg= names.c, horiz=TRUE, col = ifelse(means.c > sig.cutoffs[1] & means.c < sig.cutoffs[2], "grey85", "steelblue3"), main = "Torpor Bout Duration")
# abline(v=sig.cutoffs,lty=2,col="red")
# abline(v=0,lty=1,col="black")

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
#Create dataframe of environmental parameters (taken from our microclimate data)
env.df  <- buildEnv(temp = c(-5,20), pct.rh = c(20,100), range.res.temp = 1, range.res.rh = 1, twinter = 12, winter.res = 24)

#Create vector of species
species <- c("MYLU", "MYVE", "COTO", "EPFU", "PESU")

#Run model over parameter space per species
surv.out <- data.frame()
for(s in species){

  #Assign bat parameters
  s.params <- batLoad(bat.params, s)

  #Calculate dynamic energy over range of environmental conditions
  de.df <- data.frame(hibernationModel(env = env.df, bat.params = s.params, fung.params = fung.params))

  #Append to data frame and remove parameter names
  surv.out <- rbind(surv.out, data.frame(species=rep(s,dim(de.df)[1]),de.df))
  print(s)
}
beep()
save(surv.out, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/survResults_22March2018.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/survResults_22March2018.RData")

################################################################################
#### Trade-offs figures                                                     ####
################################################################################
#Create vectors of environmental and species data
temps <- seq(-5,20,1)
hd <- seq(20,100,1)
species <- c("MYLU", "MYVE", "COTO", "EPFU", "PESU")

#Pull fat after one month
df.fat <- data.frame()
for(t in temps){
  for(h in hd){
    #for(s in species){
      s.data <- subset(surv.out, surv.out$species == s & surv.out$Ta == t & surv.out$pct.rh == h)
      fat.inf  <- s.data[s.data$time == 24*30,]
      fat.null <- s.data[s.data$time == 24*30,]
      df <- data.frame(Species = s, Ta = t, pct.rh = h, fat.inf = fat.inf, fat.null = fat.null)
      df.fat = rbind(df.fat,df)
    #}
  }
}

#Pull survival in days
df.days <- data.frame()
for(t in temps){
  for(h in hd){
    #for(s in species){
    s.data <- subset(surv.out, surv.out$species == s & surv.out$Ta == t & surv.out$pct.rh == h)
    days.inf  <- max(s.data$time[s.data$surv.inf == 1])/24
    days.null <- max(s.data$time[s.data$surv.null == 1])/24
    df <- data.frame(Species = s, Ta = t, pct.rh = h, days.inf = days.inf, days.null = days.null)
    df.days = rbind(df.days,df)
    #}
  }
}

plot(df.days$Ta[df.days$pct.rh == 100], df.days$days.null[df.days$pct.rh == 100], col = "white", ylim = c(50,350))
lines(df.days$Ta[df.days$pct.rh == 100], df.days$days.null[df.days$pct.rh == 100], lwd = 2, col = "blue")
lines(df.days$Ta[df.days$pct.rh == 100], df.days$days.inf[df.days$pct.rh == 100], lty = 2, lwd =2, col = "blue")
lines(df.days$Ta[df.days$pct.rh == 85], df.days$days.null[df.days$pct.rh == 85],  lwd =2)
lines(df.days$Ta[df.days$pct.rh == 85], df.days$days.inf[df.days$pct.rh == 85], lty = 2, lwd =2)

plot(df.days$Ta[df.days$pct.rh == 100], df.days$days.inf[df.days$pct.rh == 100], col = "white")
lines(df.days$Ta[df.days$pct.rh == 100], df.days$days.inf[df.days$pct.rh == 100], lty = 2, lwd =2, col = "blue")
lines(df.days$Ta[df.days$pct.rh == 85], df.days$days.inf[df.days$pct.rh == 85], lty = 2, lwd =2)

ggplot(df.days[df.days$pct.rh == 100,], aes(x = Ta, y = days.null)) +
  #theme_classic() +
  geom_line(aes(x = Ta, y = days.null),  size = 1, color = "blue") +
  geom_line(aes(x = Ta, y = days.inf), linetype = 2, size = 1, color = "blue") +
  geom_line(data = df.days[df.days$pct.rh == 85,], aes(x = Ta, y = days.null), size = 1) +
  geom_line(data = df.days[df.days$pct.rh == 85,], aes(x = Ta, y = days.inf), linetype = 2, size = 1) +
  theme(panel.border = element_rect( fill = NA, color = "black")) +
  xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
  ylab("Survival (days until fat consumed)") +
  theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0))) +
  #scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme(axis.title = element_text(size = 14),#  family="serif"),
        axis.text = element_text(size = 14, color = "black")) +
  theme(plot.margin=unit(c(1,1,2,1.2),"cm"))

p1 <- ggplot(df.days[df.days$pct.rh == 100,], aes(x = Ta, y = days.null)) +
  #theme_classic() +
  #geom_line(aes(x = Ta, y = days.null),  size = 1, color = "blue") +
  ylim(c(50,150)) +
  xlim(c(0,10)) +
  geom_line(aes(x = Ta, y = days.inf), linetype = 2, size = 1, color = "blue") +
  #geom_line(data = df.days[df.days$pct.rh == 85,], aes(x = Ta, y = days.null), size = 1) +
  geom_line(data = df.days[df.days$pct.rh == 85,], aes(x = Ta, y = days.inf), linetype = 2, size = 1) +
  theme(panel.border = element_rect( fill = NA, color = "black")) +
  xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
  ylab("Days until 0 Fat") +
  theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0))) +
  #scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme(axis.title = element_text(size = 14),#  family="serif"),
        axis.text = element_text(size = 14, color = "black")) +
  theme(plot.margin=unit(c(1,1,2,1.2),"cm"))

p2 <- ggplot(df.days[df.days$pct.rh == 100,], aes(x = Ta, y = days.null)) +
  #theme_classic() +
  ylim(c(50,100)) +
  xlim(c(10,20)) +
  #geom_line(aes(x = Ta, y = days.null),  size = 1, color = "blue") +
  geom_line(aes(x = Ta, y = days.inf), linetype = 2, size = 1, color = "blue") +
  geom_line(data = df.days[df.days$pct.rh == 85,], aes(x = Ta, y = days.null), size = 1) +
  geom_line(data = df.days[df.days$pct.rh == 85,], aes(x = Ta, y = days.inf), linetype = 2, size = 1) +
  theme(panel.border = element_rect( fill = NA, color = "black")) +
  xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
  ylab("Days until 0 Fat") +
  theme(axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0))) +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme(axis.title = element_text(size = 14),#  family="serif"),
        axis.text = element_text(size = 14, color = "black")) +
  theme(plot.margin=unit(c(1,1,2,1.2),"cm"))

grid.arrange(p1,p2)

#Read in microclimate data
temp <- read.csv("C:/Users/Katie Haase/Desktop/Data/Field Data/Microclimate/microclimate_hobo_w1617.csv")
LCC <- temp[temp$id == "LCC_CATHEDRAL",]
hist(LCC$temp[LCC$temp < 5])


################################################################################
#### Plot monthly survival over parameter space                             ####
################################################################################
library(ggplot2)

#load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/months4plot_28Feb2018.RData")

#Create vectors of environmental and species data
temps <- seq(-5,20,1)
hd <- seq(20,100,1)
species <- c("MYLU", "MYVE", "COTO", "EPFU", "PESU")

#Organize survival data for plotting purposes
df.months <- data.frame()
for(t in temps){
  for(h in hd){
    for(s in species){
      s.data <- subset(surv.out, surv.out$species == s & surv.out$Ta == t & surv.out$pct.rh == h)
      months.inf  <- max(s.data$time[s.data$surv.inf == 1])/24/30
      months.null <- max(s.data$time[s.data$surv.null == 1])/24/30
      df <- data.frame(Species = s, Ta = t, pct.rh = h, Months.inf = months.inf, Months.null = months.null)
      df.months = rbind(df.months,df)
    }
  }
}

save(df.months, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/months4plot_22March2018.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/months4plot_22March2018.RData")

#Create functions for plotting over space
plot.data <- data.frame(Species = rep(df.months$Species,2), Ta = rep(df.months$Ta,2), pct.rh = rep(df.months$pct.rh,2),
                        Survival = c(df.months$Months.null,df.months$Months.inf),
                        Treatment = c(rep("Healthy", length(df.months$Months.inf)),rep("WNS",length(df.months$Months.inf))))
plot.data$Contour <- ifelse(plot.data$Survival >= 6,1,0)

plotEnvSpace <- function(plot.data, s){
  plot.sub <- plot.data[plot.data$Species == s & plot.data$pct.rh >= 60, ]
  ggplot(plot.sub, aes(Ta, pct.rh)) +
    theme_bw() +
    facet_wrap(~Treatment, ncol = 2) +
    geom_raster(aes(fill = Survival), interpolate = TRUE) +
    geom_contour(aes(z = Contour), binwidth = 1, colour = "grey15") +
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
    #theme(strip.background =element_rect(fill="white"))+
    theme(panel.spacing = unit(1, "lines")) +
    theme(axis.ticks = element_line(color = "black"))+

    scale_fill_gradientn("Months", colours = c("dodgerblue4","cadetblue1", "lightgoldenrod1"), limits = c(0, 12))
}

#Apply function to all species
p1 = plotEnvSpace(plot.data,s="MYLU")
plot(p1)
p2 = plotEnvSpace(df.months,s="myve","Myotis velfer - infected", WNS=TRUE)
p3 = plotEnvSpace(df.months,s="epfu","Eptesicus fuscus - infected", WNS=TRUE)
p4 = plotEnvSpace(df.months[df.months$pct.rh>60,],s="coto","Corynorhinus townsendii - infected", WNS=TRUE)
p5 = plotEnvSpace(df.months[df.months$pct.rh>60,],s="pesu","Perimyotis subflavus - infected", WNS=TRUE)

p11 = plotEnvSpace(df.months,s="mylu","Healthy", WNS=FALSE)
p12 = plotEnvSpace(df.months,s="myve","Myotis velfer - healthy", WNS=FALSE)
p13 = plotEnvSpace(df.months,s="epfu","Eptesicus fuscus - healthy", WNS=FALSE)
p14 = plotEnvSpace(df.months[df.months$pct.rh>60,],s="coto","Corynorhinus townsendii - healthy", WNS=FALSE)
p15 = plotEnvSpace(df.months[df.months$pct.rh>60,],s="pesu","Perimyotis subflavus - healthy", WNS=FALSE)

multiplot(p11,p1, cols = 1)
multiplot(p12,p14,p2,p4, cols = 2)

################################################################################
#### Plot Pd growth over parameter space                                    ####
################################################################################
#Load fungal growth
fung.params <- fungalSelect("Chaturvedi")

#Create dataframe of environmental parameters (taken from our microclimate data)
env.df  <- buildEnv(temp = c(-5,20), pct.rh = c(20,100), range.res.temp = 1, range.res.rh = 1, twinter = 12, winter.res = 24)

#Calculate fungal growth over environmental parameters
fung.growth = fungalGrowth(Tb = env.df$env$Ta, fung.params = fung.params, t.min = 0)*scaleFungalGrowth(pct.rh = env.df$env$pct.rh, fung.params = fung.params)*24*30
df <- data.frame(Ta = env.df$env$Ta, pct.rh = env.df$env$pct.rh, fung.growth)

p <- ggplot(df[df$pct.rh >=90 & df$Ta >=0,], aes(Ta, pct.rh)) + theme_classic() +
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
                      values = c("100"="red", "95"="orange", "90" = "green", "60" = "blue")) +
  theme_classic() +
  theme(panel.border = element_rect( fill = NA)) +
  scale_x_continuous(expand = c(0.01,0.01))+
  scale_y_continuous(expand = c(0,0.01))+
  xlab("Temperature (C)") +
  ylab(expression(paste("Daily Fungal Growth Rate (  ",cm^2,")"))) +
  theme(axis.title = element_text(size = 14,  family="serif"),
        axis.text = element_text(size = 14,  family="serif"),
        legend.title = element_text(size = 14,  family="serif"),
        legend.text = element_text(size = 14,  family="serif"))
plot(p)



################################################################################
#### Plot EWL and torpor duration over parameter space                      ####
################################################################################
#Create dataframe of environmental parameters (taken from our microclimate data)
env.df  <- buildEnv(temp = c(-5,20), pct.rh = c(20,100), range.res.temp = 1, range.res.rh = 1, twinter = 12, winter.res = 24)

#Create vector of species
species <- c("MYLU", "MYVE", "COTO", "EPFU", "PESU")

#Run EWL/TBD models over parameter space per species
for(s in species){

  #Assign bat parameters
  s.params <- batLoad(bat.params, s)

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
  save(s.out, file = paste("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/ewl&tbd_",s,"_27March2018.RData", sep = ""))
}

load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/ewl&tbd_MYLU_27March2018.RData")

#Plot over space
plot.EWL <- data.frame(Ta = rep(s.out$Ta,2), pct.rh = rep(s.out$pct.rh,2), TEWL = c(s.out$TotalEWL.null, s.out$TotalEWL.inf), CEWL = c(s.out$CEWL.null, s.out$CEWL.in),
                          PEWL = c(s.out$PEWL.null, s.out$PEWL.inf), Treatment = c(rep("Healthy", length(s.out$Ta)), rep("WNS", length(s.out$Ta))))

# plotEnvSpace <- function(s.out, s, WNS){
#   plot.sub <- s.out[s.out$Species == s, ]
#   if(WNS == TRUE){fill=plot.sub$TotalEWL.inf}else{fill=plot.sub$TotalEWL.null}
#   p <- ggplot(plot.sub, aes(Ta, pct.rh)) + theme_classic() +
#     geom_raster(aes(fill = fill), interpolate = TRUE) +
#     #ggtitle(title) +
#     theme(panel.border = element_rect( fill = NA)) +
#     theme(plot.title = element_text(face="italic")) +
#     xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
#     ylab("Relative Humidity (%)") +
#     theme(axis.title = element_text(size = 14, color = "black"),
#           axis.text = element_text(size = 14, color = "black"),
#           #plot.title = element_text(size = 18,  family="serif"),
#           #aspect.ratio = 1,
#           legend.key.size = unit(42, "points"),
#           legend.title = element_text(size = 14),
#           legend.text = element_text(size = 14)) +
#     scale_x_continuous(expand = c(0,0))+
#     scale_y_continuous(expand = c(0,0))+
#     scale_fill_gradientn(expression(paste("EWL (mg ",day^-1,")")), colours = c("dodgerblue4","cadetblue1", "lightgoldenrod1"))
# }
#
# plot(plotEnvSpace(s.out, "MYLU", WNS =TRUE))

ggplot(plot.EWL, aes(Ta, pct.rh)) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 13)) +
  theme(panel.spacing = unit(1, "lines")) +
  facet_wrap(~Treatment) +
  geom_raster(aes(fill = TEWL), interpolate = TRUE) +
  #ggtitle(title) +
  theme(panel.border = element_rect( fill = NA)) +
  xlab(expression(paste("Temperature (",degree,phantom(),C,")"))) +
  ylab("Relative Humidity (%)") +
  theme(axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 14, color = "black"),
        #plot.title = element_text(size = 18,  family="serif"),
        #aspect.ratio = 1,
        # legend.key.size = unit(42, "points"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  #theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_gradientn(expression(paste("EWL (mg ",day^-1,")")), colours = c("dodgerblue4","cadetblue1", "lightgoldenrod1"), guide = FALSE)
  #theme(plot.margin=unit(c(1,1,2,1.2),"cm"))

#Plot PEWL & CEWL
plot.EWL.95 <- subset(s.out, s.out$Species == "MYLU" & s.out$Ta >=0 & s.out$pct.rh == "95" & s.out$Time == 24)
plot.EWL.95 <- data.frame(Ta = rep(plot.EWL.95$Ta,2), pct.rh = rep(plot.EWL.95$pct.rh,2), CEWL = c(plot.EWL.95$CEWL.null, plot.EWL.95$CEWL.in),
                          PEWL = c(plot.EWL.95$PEWL.null, plot.EWL.95$PEWL.inf), Treatment = c(rep("Healthy", length(plot.EWL.95$Ta)), rep("WNS", length(plot.EWL.95$Ta))))

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
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  #theme(strip.background =element_rect(fill="slategray1")) +
  theme(strip.text.x = element_text(size = 13)) +
  theme(panel.spacing = unit(1, "lines")) +
  theme(plot.margin=unit(c(1,1,2,1.2),"cm"))

#Plot torpor bout duration








# par(mfrow=(c(2,1)))
# plot(plot.EWL$Ta, plot.EWL$CEWL.null, cex.lab = 1.1, ylim = c(0,60), col = "white", xlab = "", ylab = "Evaporative Water Loss (mg/day)")
# polygon(c(min(plot.EWL$Ta), plot.EWL$Ta, max(plot.EWL$Ta)),
#         c(0, plot.EWL$CEWL.null, 0),  col = "grey70")
# #lines(plot.EWL$Ta, plot.EWL$CEWL.null, lwd = 2)
# polygon(c(min(plot.EWL$Ta), plot.EWL$Ta, max(plot.EWL$Ta)),
#         c(0, plot.EWL$PEWL.null, 0),  col = "grey20")
# #lines(plot.EWL$Ta, plot.EWL$PEWL.null, lwd=2)
# legend(locator(1), cex = 1.2, c("Cutaneous", "Pulmonary"), bty = "n", pch =c(15,15),col=c("grey55","grey10"))
# #text(locator(1), cex=1.6, "Hd = 90%")
#
# plot(plot.EWL$Ta, plot.EWL$CEWL.in, cex.lab = 1.1, ylim = c(0,60), col = "white", xlab = expression(paste("Temperature (",degree,phantom(),C,")")), ylab = "Evaporative Water Loss (mg/day)")
# polygon(c(min(plot.EWL$Ta), plot.EWL$Ta, max(plot.EWL$Ta)),
#         c(0, plot.EWL$CEWL.in, 0),  col = "grey70")
# #lines(plot.EWL$Ta, plot.EWL$CEWL.inf, lwd = 2)
# polygon(c(min(plot.EWL$Ta), plot.EWL$Ta, max(plot.EWL$Ta)),
#         c(0, plot.EWL$PEWL.inf, 0),  col = "grey20")
# #lines(plot.EWL$Ta, plot.EWL$PEWL.inf, lwd=2)
# legend(locator(1), cex = 1.2, c("Cutaneous", "Pulmonary"), bty = "n", pch =c(15,15),col=c("grey55","grey10"))
# #text(locator(1), cex=1.6, "Hd = 90%")


################################################################################
#### Validate torpor duration with Liam's data                              ####
################################################################################

#Read in and organize files
setwd("C:/Users/Katie Haase/Desktop/Data/Skin Temp Data from Liam/")
# files <- list.files("C:/Users/Katie Haase/Desktop/Data/Skin Temp Data from Liam/")
#
# df <- data.frame()
# for(i in files){
#   csv <- read.csv(i, header= TRUE, skip = 19)
#   csv$Date.Time <- format(as.character(csv$Date.Time), format = "%d/%m/%Y %I:%M:%S %p")
#   csv$Date.Time <- as.POSIXct(csv$Date.Time, format = "%d/%m/%y %I:%M:%S %p")
#   csv$iButtonID <- strsplit(i, "_")[[1]][1]
#   csv$Bat.Type <- ifelse(strsplit(i, "_")[[1]][2] == "fungus control", "fungus", "control")
#   df <- rbind(df,csv)
# }

#Assign phase
# df$Phase[1] <- "Torpor"
# for(n in 2:nrow(df)){
#   df$Phase[n] <- ifelse(df$iButtonID[n]==df$iButtonID[n-1],
#                         ifelse(df$Value[n] <= 6.5, "Torpor",
#                                ifelse(df$Value[n] >= 25, "Euthermia",
#                                       ifelse(df$Value[n] <= df$Value[n-1], "Cooling",
#                                              ifelse(df$Value[n] >= df$Value[n-1], "Arousal", "Ignore")
#                                       )
#                                )
#                         ), "Ignore")
# }

#Check by hand
# write.csv(df, "offload_data.csv")

#Read in all temperature data after it is checked and compiled
temp.data <- read.csv("offload_data.csv", header=TRUE)
temp.data$Date.Time <- as.POSIXct(strptime(temp.data$Date.Time, format = "%m/%d/%Y %H:%M"))
temp.data <- temp.data[temp.data$Phase != "Ignore",]

#Cut to cumulative temperature data (amount of time within each phase)
temp.times <- subset(temp.data, temp.data$Subset.NoCA > 0 & temp.data$Phase != "Ignore")

#Assign first date to each file (in order to calculate Pd growth)
for(i in unique(temp.times$iButtonID)){
  temp.times$First.Date[temp.times$iButtonID == i] <- min(temp.data$Date.Time[temp.data$iButtonID == i])
}
temp.times$First.Date <- as.POSIXct(temp.times$First.Date, origin =  "1970-01-01")

#Calculate amount of time since beginning of measurements (for fungal growth estimation)
temp.times$diff.time <- as.numeric(difftime(temp.times$Date.Time, temp.times$First.Date , units = "hours"))

#Change time in phase to hours
temp.times$TimeinPhase = temp.times$Subset.NoCA/60

#Subset to torpor only
tor.data <- temp.times[temp.times$Phase == "Torpor" & temp.times$TimeinPhase > 24,]

#Read in bat parameter files
mylu.params <- batLoad(bat.params, species = "MYLU")
mylu.params$ttormax = 14*24
mylu.params$pMass.i <- 0.035

#Calculate torpor bout duration
for(i in 1:nrow(tor.data)){
  area = tor.data$diff.time[i]*fungalGrowth(Tb = 7, fung.params = fung.params, t.min = 0)*scaleFungalGrowth(pct.rh = 95, fung.params = fung.params)
  EWL <- ewl(Ta = 7, pct.rh = 98, t = tor.data$TimeinPhase[i], areaPd = area, fung.params = fung.params, bat.params = mylu.params, torpid = TRUE,  WNS = ifelse(tor.data$Bat.Type[i] == "fungus", TRUE, FALSE))
  tor.data$areaPd[i] = area
  tor.data$TotalEWL[i] = EWL$TotalEWL
  tor.data$TBD.EWL[i]   <- torporTime(Ta = 7, pct.rh = 98, areaPd = area, WNS = ifelse(tor.data$Bat.Type[i] == "fungus", TRUE, FALSE), fung.params = fung.params, bat.params = mylu.params)
}

summary(tor.data[tor.data$Bat.Type == "control",])
summary(tor.data[tor.data$Bat.Type == "fungus",])

#Perform t-test to determine significant difference
t.test(tor.data$TimeinPhase[tor.data$Bat.Type == "control"], tor.data$TBD.EWL[tor.data$Bat.Type == "control"])
t.test(tor.data$TimeinPhase[tor.data$Bat.Type == "fungus"],  tor.data$TBD.EWL[tor.data$Bat.Type == "fungus"])

#Define pmass threshold from data
#(TBD*EWL)/(lean*1000)
tor.data$EWLhrly <- tor.data$TotalEWL/tor.data$TimeinPhase
tor.data$Threshold = tor.data$TotalEWL/((tor.data$Mass)*1000)

#Plot pmass thresold against Pd growth per individual to determine any relationship
id = unique(tor.data$iButtonID)[16]
plot(tor.data$areaPd[tor.data$iButtonID == id], tor.data$Threshold[tor.data$iButtonID == id])
plot(tor.data$areaPd[tor.data$Bat.Type == "fungus"], tor.data$Threshold[tor.data$Bat.Type == "fungus"])
plot(tor.data$areaPd[tor.data$Bat.Type == "control"], tor.data$Threshold[tor.data$Bat.Type == "control"])

################################################################################
#### Validate EWL model with new data                                       ####
################################################################################
#Read in data and subset to dry EWL data only
measured.EWL <- read.csv("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Input Files/TMR.csv")
measured.EWL <- measured.EWL[measured.EWL$Treatment == "dry" & is.na(measured.EWL$EWL) == FALSE,]

#Determine species
species <- c("MYVE","PESU","COTO","EPFU","MYLU")
#bat.params$rEWL = (bat.params$rEWL*(10*(bat.params$mass^0.67)))/bat.params$SA.wing

#Calculate EWL for each individual measurement
ewl.df <- data.frame()
for(s in 1:5){
  s.params <- batLoad(bat.params, species = species[s])
  s.params$TMRmin = measured.EWL$TMRO2.g

  s.data <- measured.EWL[measured.EWL$Species == species[s],]

  df <- data.frame(Species = species[s], Measured.EWL = s.data$EWL, ewl(Ta = s.data$act_temp, pct.rh = 10, t = 1, areaPd = 0, torpid = TRUE, WNS = FALSE, fung.params = fung.params, bat.params = s.params))

  ewl.df <- rbind(ewl.df, df)
}

#Compare measured and modeled EWL with a t-test to test significant difference
t.test(ewl.df$Measured.EWL, ewl.df$TotalEWL)
t.test(ewl.df$Measured.EWL[ewl.df$Species == "MYLU"], ewl.df$TotalEWL[ewl.df$Species == "MYLU"])

ewl.df[ewl.df$Species == "MYLU",]
################################################################################
#### Validate survival model with Liam's/Jonasson's data                    ####
################################################################################
#Read in bat data
mylu.params = batLoad(bat.params, "MYLU")

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
  i.params = mylu.params
  i.params$Mass = mass.data$Begin.Mass[mass.data$iButton == id]

  #Determine average arousal, cooling, and euthermic time for bat
  arousal <- mean(phase$TimeinPhase[phase$iButtonID == id & phase$Phase.NoCA == "Arousal"])
  cooling <- mean(phase$TimeinPhase[phase$iButtonID == id & phase$Phase.NoCA == "Cooling"])
  euthermia <- mean(phase$TimeinPhase[phase$iButtonID == id & phase$Phase.NoCA == "Euthermia"])

  #Determine total time in torpor
  d.data$Date <- as.Date.character(as.character(d.data$Date), format = "%m/%d/%Y")
  torpor <- sum(c(0,difftime(d.data$Date[2:length(d.data$Date)], d.data$Date[1:(length(d.data$Date)-1)], units = "hours")))

  #Subtract arousal, cooling, and euthermia time per torpor bout
  torpor <- torpor - ((arousal + cooling + euthermia)*length(d.data$Date))

  #Calculate total energy based on torpor, arousal, cooling, and euthermia time
  arousal.E <- arousalEnergy(Ta = 7, bat.params = i.params)*arousal*length(d.data$Date)
  cooling.E <- coolEnergy(Ta = 7, bat.params = i.params)*cooling*length(d.data$Date)
  euthermia.E <- euthermicEnergy(Ta = 7, bat.params = i.params)*euthermia*length(d.data$Date)
  torpor.E <- torporEnergy(Ta = 7, bat.params = i.params, q = calcQ(7))*torpor

  #Sum all energy and convert to fat loss
  mass.data$FatConsumed[mass.data$iButton == id] <- ((arousal.E + cooling.E + euthermia.E + torpor.E)*20.1)/(39.3*1000)

}

t.test(mass.data$FatConsumed, mass.data$Weight.Loss)
#
# #Calculate fat loss per phase
# for(i in unique(phase$iButtonID)){
#   i.data = subset(phase, phase$iButtonID == i & as.Date.character(as.character(phase$Date.Time), format = "%m/%d/%Y") <= as.Date.character(as.character(mass.data$EndDate[mass.data$iButton == i]), format = "%m/%d/%Y"))
#   i.params = mylu.params
#   i.params$mass = i.data$Mass
#
#   for(p in 1:dim(i.data)[1]){
#     if(i.data$Phase[p] == "Torpor"){
#       i.data$EnergyConsumed[p] <- (torporEnergy(Ta = 7, bat.params = i.params, q = calcQ(7))*i.data$TimeinPhase[p])
#     } else if(i.data$Phase[p] == "Arousal"){
#       i.data$EnergyConsumed[p] <- (arousalEnergy(Ta = 7, bat.params = i.params)*i.data$TimeinPhase[p])
#     } else if(i.data$Phase[p] == "Euthermia"){
#       i.data$EnergyConsumed[p] <- (euthermicEnergy(Ta = 7, bat.params = i.params)*i.data$TimeinPhase[p])
#     } else{
#       i.data$EnergyConsumed[p] <- (coolEnergy(Ta = 7, bat.params = i.params)*i.data$TimeinPhase[p])
#     }
#   }
#   mass.data$FatConsumed[mass.data$iButton == i] <- (sum(i.data$EnergyConsumed)*20.1)/(39.3*1000)
# }


#Jonasson Validation
data <- read.csv("C:/Users/Katie Haase/Desktop/Data/Skin Temp Data from Liam/JonassonValidation.csv")
data$diff <- abs(as.numeric(difftime(as.Date.character(as.character(data$StartDate), format = "%m/%d/%Y"), as.Date.character(as.character(data$EndDate),format = "%m/%d/%Y"))))
env.df  <- buildEnv(temp = c(6), pct.rh = c(85,85), range.res.temp = 1, range.res.rh = 1, twinter = 2, winter.res = 24)
mylu.params <- batLoad(bat.params, "mylu")
for(i in 1:12){
  i.params = mylu.params
  i.params$mass = data$StartMass[i]
  surv <- hibernationModel(env = env.df, bat.params = i.params, fung.params = fung.params)
  data$fat[i] <- surv$n.g.fat.consumed[length(surv$time[surv$surv.inf == 1])]
}
data$WeightLoss <- data$StartMass - data$EndMass

t.test(data$WeightLoss, data$fat)

################################################################################
#### Run model to include phenotypic & environmental variation              ####
################################################################################
#Pull body mass from distrbution

#Determine need to change locations based on body fat?

#pull temprature from distrbution - mean and sd from data



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
``
