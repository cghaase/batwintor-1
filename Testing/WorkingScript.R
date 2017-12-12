library(batwintor)
library(grofit)
library(ggplot2)
library(testthat)
library(deSolve)
library(data.table)
library(dplyr)
library(beepr)library(devtools)
#devtools::document()

#Read in bat data
data("bat.params")

#Load fungal growth
fung.params <- FungSelect("Chaturvedi")

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
#### Fit species-specific lines to rEWL & body mass (revist with more data) ####
################################################################################
#Read in EWL/TMR data
TMR <- read.csv("C:/Users/Katie Haase/Desktop/Current/TMR.csv")

#Cut to dry measurements data only
TMR <- TMR[is.na(TMR$EWL) == FALSE & TMR$Treatment == "dry",]

#Calculate mean per individual
EWL.mean <- data.frame(EWL.mean = tapply(TMR$EWL, INDEX = as.factor(as.character(TMR$Bat.ID)), FUN = mean, na.rm=TRUE))

#Recreate data.frame
df.ewl <- data.frame(BatID = rownames(EWL.mean), Species = TMR$Bat.Species[match(rownames(EWL.mean), TMR$Bat.ID)], EWL.mean = EWL.mean, Mass = TMR$Mass..prior.resp.[match(rownames(EWL.mean), TMR$Bat.ID)])

#Calculate area-specific EWL
df.ewl$rEWL <- df.ewl$EWL.mean/(10*(df.ewl$Mass^(2/3)))

#Fit linear model per species
df.mod <- data.frame()
for(s in unique(TMR$Bat.Species)){
  lm.data <- df.ewl[df.ewl$Species == s,]
  plot(lm.data$Mass, lm.data$rEWL)
  mod <- lm(log(rEWL) ~ log(Mass), data = lm.data)
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
mylu.params <- BatLoad(bat.params, "mylu")

#Determine number of bins/intervals for PRCC
nspaces=100

#Create a LHS function with the N of columns that matches parameters
hypercube=randomLHS(n=nspaces, k=27)

#Create range of parameters (k has to equal number listed)
mins = with(c(as.list(mylu.params),as.list(fung.params)),
            c(   			    ## set mins for each parameters-exclude those not to be varied if any
              mass = 0.75*7.8,
              RMR = 0.75*2.6,
              TMRmin = 0.25*0.0153,
              Teu = 30,
              Tlc = 34,
              Ttormin = 2,
              Ceu = 0.75*0.2638,
              Ct = 0.75*0.055,
              S = 0.75*0.1728,
              ttormax = 0.5*792,
              teu = 0.45,
              WR = 0.5*48,
              CR = 0.5*32.16,
              rEWL = 0.5*0.1826,
              mrPd = 0.5*1.4,
              aPd = 0.5*0.21,
              rPd = 0.5*1.525,
              pMass.i = 0.5*0.043,
              pMass = 0.5*0.007,
              pFly = 0,
              pLean = 0.5*0.532,
              pFat = 0.5*0.216,
              beta1 = 0.75*0.0007751467,
              beta2 = 0.75*0.2699683,
              beta3 = 0.75*19.7309,
              mu1 = 0.75*0.000150958,
              mu2 = 0.75*-0.009924594
        ))

maxs = with(c(as.list(mylu.params),as.list(fung.params)),
            c( 				    ## set maxs for each parameters-exclude those not to be varied if any
              mass = 1.25*7.8,
              RMR = 1.25*2.6,
              TMRmin = 1.75*0.0153,
              Teu = 40,
              Tlc = 30,
              Ttormin = 8,
              Ceu = 1.25*0.2638,
              Ct = 1.25*0.055,
              S = 1.25*0.1728,
              ttormax = 1.5*792,
              teu = 48,
              WR = 1.5*48,
              CR = 1.5*32.16,
              rEWL = 1.5*0.1826,
              mrPd = 1.5*1.4,
              aPd = 1.5*0.21,
              rPd = 1.5*1.525,
              pMass.i = 1.5*0.043,
              pMass = 1.5*0.007,
              pFly = 100,
              pLean = 1.5*0.532,
              pFat = 1.5*0.216,
              beta1 = 1.25*0.0007751467,
              beta2 = 1.25*0.2699683,
              beta3 = 1.25*19.7309,
              mu1 = 1.25*0.000150958,
              mu2 = 1.25*-0.009924594
      ))

diffs=maxs-mins ## range of each variable

#Create copy of hypercube samples to modify, hypercube adjusted; i.e. new matrix
hypercubeadj = hypercube
for (i in 1:ncol(hypercube)){
  hypercubeadj[,i]=hypercube[,i]*diffs[i]+mins[i] # scale samples to difference and add minimum
}

#Give names of parameters -- makes sure the same order as above
dimnames(hypercubeadj)[[2]]=c(
                              "mass",
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
                              "beta1",
                              "beta2",
                              "beta3",
                              "mu1",
                              "mu2"
          )

paramset<-hypercubeadj

#Determine environment to run model over
env.df  <- BuildEnv(temp = c(2,20), pct.rh = c(60,100), range.res.temp = 5, range.res.rh = 5, twinter = 10, winter.res = 1)

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
    env.df  <- BuildEnv(temp = c(env$Ta,env$Ta), pct.rh = c(env$pct.rh,env$pct.rh), range.res.temp = 1, range.res.rh = 1, twinter = 10, winter.res = 1)
    res.a = DynamicEnergyPd(env = env.df, bat.params = paramset[i,], fung.params = paramset[i,])

    #Subset results to single values
    res.a = max(res.a$time[res.a$surv.inf == 1])

    #Calculate fungal growth rate over an hour (for use in other 2 functions)
    area = FungalGrowthRate(Tb = paramset.H$env[j,1], fung.params = as.list(paramset[i,]), t.min = 0)*ScaleFungalGrowthRate(pct.rh = paramset.H$env[j,2], fung.params = as.list(paramset[i,]))

    #Calculate EWL over an hour
    res.b = CalcEWL(Ta=paramset.H$env[j,1], pct.rh=paramset.H$env[j,2], areaPd = area*24,  t=24, mod.params = as.list(paramset[i,]), torpid = TRUE, WNS = TRUE)$TotalEWL

    #Calculate torpor duration at end of winter (time for Pd growth is max survival time from res.a)
    res.c = CalcTorporTimePd(Ta=paramset.H$env[j,1], pct.rh=paramset.H$env[j,2], WNS = TRUE, mod.params = as.list(paramset[i,]), areaPd = area*res.a)

    #Fill in results matrix
    res_out.a[j,] <- res.a
    res_out.b[j,] <- res.b
    res_out.c[j,] <- res.c
  }

  sen.results.a[i,]<-as.vector(t(res_out.a))
  sen.results.b[i,]<-as.vector(t(res_out.b))
  sen.results.c[i,]<-as.vector(t(res_out.c))
}
save(sen.results.a, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_surv_12Dec2017.RData")
save(sen.results.b, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_ewl_12Dec2017.RData")
save(sen.results.c, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_tor_12Dec2017.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_surv.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_ewl.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_tor.RData")

#Apply function to new model results
PRCCresults.a <- prcc(par.mat = paramset[,-9], model.output = sen.results.a[,-9], routine = "blower", par.names = colnames(paramset[,-9]),output.names = seq(1,ncol(sen.results.a[,-9])))
PRCCresults.b <- prcc(par.mat = paramset[,-9], model.output = sen.results.b[,-9], routine = "blower", par.names = colnames(paramset[,-9]),output.names = seq(1,ncol(sen.results.b[,-9])))
PRCCresults.c <- prcc(par.mat = paramset[,-9], model.output = sen.results.c[,-9], routine = "blower", par.names = colnames(paramset[,-9]),output.names = seq(1,ncol(sen.results.c[,-9])))

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
means.a <- data.frame(means=PRCCresults.a[[1]][,1])
means.b <- data.frame(means=PRCCresults.b[[1]][c(1,3,9,13:16,22:26),1])
means.c <- data.frame(means=PRCCresults.c[[1]][c(1,3:6,9,13:18,22:26),1])

#Plot mean with significance
op <- par(family = "serif")
names.a = expression("Body Mass","RMR","TMR"[min],"T"[eu],"T"[lc],"T"[tormin],"C"[eu],"C"[tor],"t"[tormax],"t"[eu],"Warming Rate","Cooling Rate","Rate of EWL",
          "TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd","EWL Threshold"[infected],"EWL Threshold"[healthy],"% Euthermia Spent Flying","% Body Mass at Lean","% Body Mass as Fat",beta[1],beta[2],beta[3],mu[1],mu[2])
names.b = expression("Body Mass","TMR"[min],"t"[tormax],"Rate of EWL","TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd",beta[1],beta[2],beta[3],mu[1],mu[2])
names.c = expression("Body Mass","TMR"[min],"T"[eu],"T"[lc],"T"[tormin],"t"[tormax],"Rate of EWL","TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd","EWL Threshold"[infected],"EWL Threshold"[healthy],beta[1],beta[2],beta[3],mu[1],mu[2])

par(mar=c(5.1,15,4.1,2.1))
barplot(means.a[,1], xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
        names.arg= names.a, horiz=TRUE, col = "grey85", main = "Survival")
t.cutoff=qt(0.05/2,df=100-2)
sig.cutoffs=c( (-(t.cutoff^2)-sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)),
               (-(t.cutoff^2)+sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)))
abline(v=sig.cutoffs,lty=2,col="red")
abline(v=0,lty=1,col="black")

barplot(means.b[,1], xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
        names.arg= names.b, horiz=TRUE, col = "grey85", main = "Total Evaporative Water Loss")
t.cutoff=qt(0.05/2,df=100-2)
sig.cutoffs=c( (-(t.cutoff^2)-sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)),
               (-(t.cutoff^2)+sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)))
abline(v=sig.cutoffs,lty=2,col="red")
abline(v=0,lty=1,col="black")

barplot(means.c[,1], xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
        names.arg= names.c, horiz=TRUE, col = "grey85", main = "Torpor Bout Duration")
t.cutoff=qt(0.05/2,df=100-2)
sig.cutoffs=c( (-(t.cutoff^2)-sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)),
               (-(t.cutoff^2)+sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)))
abline(v=sig.cutoffs,lty=2,col="red")
abline(v=0,lty=1,col="black")

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

#Read in bat data
data("bat.params")

#Load fungal growth
fung.params <- FungSelect("Chaturvedi")

#Create dataframe of environmental parameters (taken from our microclimate data)
env.df  <- BuildEnv(temp = c(-5,20), pct.rh = c(20,100), range.res.temp = 1, range.res.rh = 5, twinter = 10, winter.res = 24)

#Create vector of species
species <- c("mylu", "myve", "coto", "epfu", "pesu")

#Run model over parameter space per species
surv.out <- data.frame()
for(s in species){

  #Assign bat parameters
  s.params <- BatLoad(bat.params, s)

  #Calculate dynamic energy over range of environmental conditions
  de.df <- data.frame(DynamicEnergyPd(env = env.df, bat.params = s.params, fung.params = fung.params))

  #Append to data frame and remove parameter names
  surv.out <- rbind(surv.out, data.frame(species=rep(s,dim(de.df)[1]),de.df))
}
save(surv.out, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/survResults_11Dec2017.RData")

################################################################################
#### Plot monthly survival over parameter space                             ####
################################################################################
library(ggplot2)

load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/survResults_11Dec2017.RData")

#Create vectors of environmental and species data
temps <- seq(-5,20,1)
hd <- seq(20,100,5)
species <- c("mylu", "myve", "coto", "epfu", "pesu")

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

save(df.months, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/months4plot.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/months4plot.RData")

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

#Create function for plotting over space
plotEnvSpace <- function(df.months, s, title, WNS){
  plot.sub <- df.months[df.months$Species == s, ]
  if(WNS == TRUE){fill=plot.sub$Months.inf}else{fill=plot.sub$Months.null}
  p <- ggplot(plot.sub, aes(Ta, pct.rh)) + theme_classic() +
    geom_raster(aes(fill = fill), interpolate = TRUE) +
    ggtitle(title) +
    theme(plot.title = element_text(face="italic")) +
    xlab("Temperature (C)") +
    ylab("Relative Humidity (%)") +
    scale_fill_gradientn(colours = terrain.colors(10), limits = c(0, 10))
    #geom_polygon(data = polygon[polygon$Site == "SLL",], aes(x=X, y=Y), colour="black", fill=NA)
  #plot(p)
}


#Apply function to all species
p1 = plotEnvSpace(df.months,s="mylu","Myotis lucifugus - infected", WNS=TRUE)
p2 = plotEnvSpace(df.months,s="myve","Myotis velfer - infected", WNS=TRUE)
p3 = plotEnvSpace(df.months,s="epfu","Eptesicus fuscus - infected", WNS=TRUE)
p4 = plotEnvSpace(df.months,s="coto","Corynorhinus townsendii - infected", WNS=TRUE)
p5 = plotEnvSpace(df.months,s="pesu","Perimyotis subflavus - infected", WNS=TRUE)

p11 = plotEnvSpace(df.months,s="mylu","Myotis lucifugus - healthy", WNS=FALSE)
p12 = plotEnvSpace(df.months,s="myve","Myotis velfer - healthy", WNS=FALSE)
p13 = plotEnvSpace(df.months,s="epfu","Eptesicus fuscus - healthy", WNS=FALSE)
p14 = plotEnvSpace(df.months,s="coto","Corynorhinus townsendii - healthy", WNS=FALSE)
p15 = plotEnvSpace(df.months,s="pesu","Perimyotis subflavus - healthy", WNS=FALSE)

multiplot(p1,p3,p11,p13, cols = 2)
multiplot(p2,p4,p12,p14, cols = 2)



################################################################################
#### Validate torpor duration                                               ####
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
temp.times <- subset(temp.data, temp.data$Subset > 0 & temp.data$Phase != "Ignore")
temp.times.NoCA <- subset(temp.data, temp.data$Subset.NoCA > 0 & temp.data$Phase != "Ignore")

#Assign first date to each file (in order to calculate Pd growth)
for(i in unique(temp.times$iButtonID)){
  temp.times$First.Date[temp.times$iButtonID == i] <- min(temp.data$Date.Time[temp.data$iButtonID == i])
  temp.times.NoCA$First.Date[temp.times.NoCA$iButtonID == i] <- min(temp.data$Date.Time[temp.data$iButtonID == i])
}
temp.times$First.Date <- as.POSIXct(temp.times$First.Date, origin =  "1970-01-01")
temp.times.NoCA$First.Date <- as.POSIXct(temp.times.NoCA$First.Date, origin =  "1970-01-01")

#Calculate amount of time since beginning of measurements (for fungal growth estimation)
temp.times$diff.time <- as.numeric(difftime(temp.times$Date.Time, temp.times$First.Date, units = "hours"))
temp.times.NoCA$diff.time <- as.numeric(difftime(temp.times.NoCA$Date.Time, temp.times.NoCA$First.Date, units = "hours"))

#Change time in phase to hours
temp.times$TimeinPhase = temp.times$Subset/60
temp.times.NoCA$TimeinPhase = temp.times.NoCA$Subset.NoCA/60

#Subset to torpor only
tor.data <- temp.times[temp.times$Phase == "Torpor" & temp.times$TimeinPhase > 24,]
tor.data.NoCA <- temp.times.NoCA[temp.times.NoCA$Phase == "Torpor" & temp.times.NoCA$TimeinPhase > 24,]

#Read in bat parameter files
data("bat.params")
mylu.params <- BatLoad(bat.params, species = "mylu")
mylu.params$pMass = 0.007

#Load fungal parameters
fung.params <- FungSelect("Chaturvedi")

#Calculate torpor bout duration
for(i in 1:nrow(tor.data.NoCA)){
  area = tor.data.NoCA$diff.time[i]*FungalGrowthRate(Tb = 7, fung.params = fung.params, t.min = 0)*ScaleFungalGrowthRate(pct.rh = 98, fung.params = fung.params)
  EWL <- CalcEWL(Ta = 7, pct.rh = 98, t = tor.data.NoCA$TimeinPhase[i], areaPd = area, mod.params = c(fung.params,mylu.params), torpid = TRUE,  WNS = ifelse(tor.data.NoCA$Bat.Type[i] == "fungus", TRUE, FALSE))
  tor.data.NoCA$areaPd[i] = area
  tor.data.NoCA$TotalEWL[i] = EWL$TotalEWL
  tor.data.NoCA$TBD.EWL[i]   <- CalcTorporTimePd(Ta = 7, pct.rh = 98, areaPd = area, WNS = ifelse(tor.data.NoCA$Bat.Type[i] == "fungus", TRUE, FALSE), mod.params = c(fung.params,mylu.params))
}

summary(tor.data.NoCA[tor.data.NoCA$Bat.Type == "control",])
summary(tor.data.NoCA[tor.data.NoCA$Bat.Type == "fungus",])

#Perform t-test to determine significant difference
t.test(tor.data.NoCA$TimeinPhase[tor.data.NoCA$Bat.Type == "fungus"],  tor.data.NoCA$TBD.EWL[tor.data.NoCA$Bat.Type == "fungus"])
t.test(tor.data.NoCA$TimeinPhase[tor.data.NoCA$Bat.Type == "control"], tor.data.NoCA$TBD.EWL[tor.data.NoCA$Bat.Type == "control"])

#Define pmass threshold from data
#(TBD*EWL)/(lean*1000)
tor.data.NoCA$EWLhrly <- tor.data.NoCA$TotalEWL/tor.data.NoCA$TimeinPhase
tor.data.NoCA$Threshold = (tor.data.NoCA$TimeinPhase*tor.data.NoCA$EWLhrly)/((tor.data.NoCA$Mass*mylu.params$pLean)*1000)

#Plot pmass thresold against Pd growth per individual to determine any relationship
id = unique(tor.data.NoCA$iButtonID)[16]
plot(tor.data.NoCA$areaPd[tor.data.NoCA$iButtonID == id], tor.data.NoCA$Threshold[tor.data.NoCA$iButtonID == id])
plot(tor.data.NoCA$areaPd[tor.data.NoCA$Bat.Type == "fungus"], tor.data.NoCA$Threshold[tor.data.NoCA$Bat.Type == "fungus"])
plot(tor.data.NoCA$areaPd[tor.data.NoCA$Bat.Type == "control"], tor.data.NoCA$Threshold[tor.data.NoCA$Bat.Type == "control"])

################################################################################
#### Validate EWL model with new data                                       ####
################################################################################
#Read in data and subset to dry EWL data only
measured.EWL <- read.csv("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Input Files/TMR.csv")
measured.EWL <- measured.EWL[measured.EWL$Treatment == "dry" & is.na(measured.EWL$EWL) == FALSE,]

#Determine species
measured.species <- unique(measured.EWL$Bat.Species)
species <- c("myve","pesu","coto","epfu","mylu")

#Calculate EWL for each individual measurement
ewl.df <- data.frame()
for(s in 1:5){
  s.params <- BatLoad(bat.params, species = species[s])

  s.data <- measured.EWL[measured.EWL$Bat.Species == measured.species[s],]

  df <- data.frame(Species = species[s], Measured.EWL = s.data$EWL, CalcEWL(Ta = s.data$Actual.Temperature, pct.rh = 0, t = 1, areaPd = 0, torpid = TRUE, WNS = FALSE, mod.params = c(fung.params, s.params)))

  ewl.df <- rbind(ewl.df, df)
}

#Compare measured and modeled EWL with a t-test to test significant difference
t.test(ewl.df$Measured.EWL, ewl.df$TotalEWL)


