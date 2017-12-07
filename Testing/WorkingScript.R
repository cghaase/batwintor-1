library(batwintor)
library(grofit)
library(ggplot2)
library(testthat)
library(devtools)
devtools::document()

#Read in bat data
data("bat.params")

#Load fungal growth
fung.params <- FungSelect("Chaturvedi")

#### Determine proportion of euthermia spent flying ####
#Should match 88 & 114 days [Warnecke et al 2012 PNAS]
mylu.params <- BatLoad(bat.params, "mylu")

#Build environment
env.df = BuildEnv(temp = c(7,7), pct.rh = c(95,100), range.res = 1)
twinter <- seq(from = 0, to = 9 * 24 * 30 * 1, by = 24)  #Maximum time of winter is 9 months, broken at daily intervals

#Allow prop of flying to vary
s <- seq(0.05, 1, 0.05)
df <- data.frame()
for(f in s){
  mylu.params$pFly = f
  mylu.mod <- DynamicEnergyPd(env.df = env.df,  bat.params = mylu.params, fung.params = fung.params)
  mylu.mod.null = mylu.mod[mylu.mod$Ta == 7 & mylu.mod$humidity == 97 & mylu.mod$surv.null == 1]
  mylu.mod.null = mylu.mod.null[mylu.mod.null$time == max(mylu.mod.null$time)]
  mylu.mod.inf = mylu.mod[mylu.mod$Ta == 7 & mylu.mod$humidity == 97 & mylu.mod$surv.inf == 1]
  mylu.mod.inf = mylu.mod.inf[mylu.mod.inf$time == max(mylu.mod.inf$time)]
  all.df <- data.frame(Ta = mylu.mod.null$Ta, humidity = mylu.mod.null$humidity, pFly = mylu.params$pFly, n.g.fat.consumed = mylu.mod.null$n.g.fat.consumed, NullTime = mylu.mod.null$time,
                       g.fat.consumed = mylu.mod.inf$g.fat.consumed, InfectTime = mylu.mod.inf$time)
  df <- rbind(df, all.df)
}



#### Fit species-specific lines to rEWL & body mass (revist with more data) ####

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


#### Sensitivity analysis ####
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
              TMRmin = 0.75*0.0153,
              Teu = 0.75*35,
              Tlc = 0.75*32,
              Ttormin = 0.75*5,
              Ceu = 0.75*0.2638,
              Ct = 0.75*0.055,
              S = 0.75*0.1728,
              ttormax = 0.75*792,
              teu = 0.75*3,
              WR = 0.75*48,
              CR = 0.75*32.16,
              rEWL = 0.75*0.1826,
              mrPd = 0.75*1.4,
              aPd = 0.75*0.21,
              rPd = 0.75*1.525,
              pMass.i = 0.75*0.043,
              pMass = 0.75*0.007,
              pFly = 0.75*0.35,
              pLean = 0.75*0.532,
              pFat = 0.75*0.216,
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
              TMRmin = 1.25*0.0153,
              Teu = 1.25*35,
              Tlc = 1.25*32,
              Ttormin = 1.25*5,
              Ceu = 1.25*0.2638,
              Ct = 1.25*0.055,
              S = 1.25*0.1728,
              ttormax = 1.25*792,
              teu = 1.25*3,
              WR = 1.25*48,
              CR = 1.25*32.16,
              rEWL = 1.25*0.1826,
              mrPd = 1.25*1.4,
              aPd = 1.25*0.21,
              rPd = 1.25*1.525,
              pMass.i = 1.25*0.043,
              pMass = 1.25*0.007,
              pFly = 1.25*0.35,
              pLean = 1.25*0.532,
              pFat = 1.25*0.216,
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
env.df  <- BuildEnv(temp = c(2,20), pct.rh = c(60,100), range.res = 5)
twinter <- seq(from = 0, to = 9 * 24 * 30 * 1, by = 24)  #Maximum time of winter is 9 months, broken at daily intervals

#Create matrix of parameter values over environment
paramset.H <- env.df
sen.results.a <-matrix(NA,nrow=length(paramset[,1]),ncol=nrow(paramset.H))
sen.results.b <-matrix(NA,nrow=length(paramset[,1]),ncol=nrow(paramset.H))
sen.results.c <-matrix(NA,nrow=length(paramset[,1]),ncol=nrow(paramset.H))

#Run model over environment & parameter space
for (i in 1:length(paramset[,1])){

  res_out.a <- matrix(0, nrow(paramset.H),1)
  res_out.b <- matrix(0, nrow(paramset.H),1)
  res_out.c <- matrix(0, nrow(paramset.H),1)

  for (j in 1:nrow(paramset.H)) {
    #Calculate survival time
    res.a = DynamicEnergyPd(env.df = paramset.H[j,], bat.params = paramset[i,], fung.params = paramset[i,])

    #Subset results to single values
    res.a = max(res.a$time[res.a$surv.inf == 1])

    #Calculate fungal growth rate over an hour (for use in other 2 functions)
    area = FungalGrowthRate(Tb = paramset.H$Ta[j], fung.params = as.list(paramset[i,]), t.min = 0)*ScaleFungalGrowthRate(pct.rh = paramset.H$pct.rh[j], fung.params = as.list(paramset[i,]))

    #Calculate EWL over an hour
    res.b = CalcEWL(Ta=paramset.H$Ta[j], pct.rh=paramset.H$pct.rh[j], areaPd = area*24,  t=24, mod.params = as.list(paramset[i,]), torpid = TRUE, WNS = TRUE)$TotalEWL

    #Calculate torpor duration at end of winter (time for Pd growth is max survival time from res.a)
    res.c = CalcTorporTimePd(Ta=paramset.H$Ta[j], pct.rh=paramset.H$pct.rh[j], WNS = TRUE, mod.params = as.list(paramset[i,]), areaPd = area*res.a)

    #Fill in results matrix
    res_out.a[j,] <- res.a
    res_out.b[j,] <- res.b
    res_out.c[j,] <- res.c

   }

  sen.results.a[i,]<-as.vector(t(res_out.a))
  sen.results.b[i,]<-as.vector(t(res_out.b))
  sen.results.c[i,]<-as.vector(t(res_out.c))
}

#Apply function to new model results
PRCCresults.a <- prcc(par.mat = paramset, model.output = sen.results.a, routine = "blower", par.names = colnames(paramset),output.names = seq(1,ncol(sen.results.a)))
PRCCresults.b <- prcc(par.mat = paramset, model.output = sen.results.b, routine = "blower", par.names = colnames(paramset),output.names = seq(1,ncol(sen.results.b)))
PRCCresults.c <- prcc(par.mat = paramset, model.output = sen.results.c, routine = "blower", par.names = colnames(paramset),output.names = seq(1,ncol(sen.results.c)))

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
means.b <- data.frame(means=PRCCresults.b[[1]][c(1,3,10,14:17,23:27),1])
means.c <- data.frame(means=PRCCresults.c[[1]][c(1,3:6,10,14:19,23:27),1])

#Plot mean with significance
op <- par(family = "serif")
names.a = expression("Body Mass","RMR","TMR"[min],"T"[eu],"T"[lc],"T"[tormin],"C"[eu],"C"[tor],"Heat capacity of tissue","t"[tormax],"t"[eu],"Warming Rate","Cooling Rate","Rate of EWL",
          "TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd","EWL Threshold"[infected],"EWL Threshold"[healthy],"% Euthermia Spent Flying","% Body Mass at Lean","% Body Mass as Fat",beta[1],beta[2],beta[3],mu[1],mu[2])
names.b = expression("Body Mass","TMR"[min],"t"[tormax],"Rate of EWL","TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd",beta[1],beta[2],beta[3],mu[1],mu[2])
names.c = expression("Body Mass","TMR"[min],"T"[eu],"T"[lc],"T"[tormin],"t"[tormax],"Rate of EWL","TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd","EWL Threshold"[infected],"EWL Threshold"[healthy],beta[1],beta[2],beta[3],mu[1],mu[2])

par(mar=c(5.1,15,4.1,2.1))
barplot(means.a$means, xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
        names.arg= names.a, horiz=TRUE, col = "grey85", main = "Survival")
t.cutoff=qt(0.05/2,df=100-2)
sig.cutoffs=c( (-(t.cutoff^2)-sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)),
               (-(t.cutoff^2)+sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)))
abline(v=sig.cutoffs,lty=2,col="red")
abline(v=0,lty=1,col="black")

barplot(means.b$means, xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
        names.arg= names.b, horiz=TRUE, col = "grey85", main = "Total Evaporative Water Loss")
t.cutoff=qt(0.05/2,df=100-2)
sig.cutoffs=c( (-(t.cutoff^2)-sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)),
               (-(t.cutoff^2)+sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)))
abline(v=sig.cutoffs,lty=2,col="red")
abline(v=0,lty=1,col="black")

barplot(means.c$means, xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
        names.arg= names.c, horiz=TRUE, col = "grey85", main = "Total Evaporative Water Loss")
t.cutoff=qt(0.05/2,df=100-2)
sig.cutoffs=c( (-(t.cutoff^2)-sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)),
               (-(t.cutoff^2)+sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)))
abline(v=sig.cutoffs,lty=2,col="red")
abline(v=0,lty=1,col="black")



#### Fit other functions to fungal growth ####
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
