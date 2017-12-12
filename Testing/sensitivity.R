library(lhs)

#Assign bat parameters
mylu.params <- BatLoad(bat.params, "mylu")

#Determine number of bins/intervals for PRCC
nspaces=100

#Create a LHS function with the k columns that matches # of parameters
hypercube=randomLHS(n=nspaces, k=27)

#Create range of parameters (k has to equal # of parameters listed)
mins = with(c(as.list(mylu.params),as.list(fung.params)),
            c(   			    ## set mins for each parameters-exclude those not to be varied if any (if they don't vary it doesn't work)
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

#Give names of parameters -- makes sure the same order as above!
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
env.df  <- BuildEnv(temp = c(2,20), pct.rh = c(60,100), range.res.temp = 5, range.res.rh = 5, twinter = 10, winter.res = 24)

#Create matrix of parameter values over environment
paramset.H <- env.df
sen.results.surv <-matrix(NA,nrow=length(paramset[,1]),ncol=nrow(paramset.H$env))
sen.results.ewl  <-matrix(NA,nrow=length(paramset[,1]),ncol=nrow(paramset.H$env))
sen.results.tbd  <-matrix(NA,nrow=length(paramset[,1]),ncol=nrow(paramset.H$env))

#Run model over environment & parameter space
for (i in 1:length(paramset[,1])){

  res_out.surv <- matrix(0, nrow(paramset.H$env),1)
  res_out.ewl  <- matrix(0, nrow(paramset.H$env),1)
  res_out.tbd  <- matrix(0, nrow(paramset.H$env),1)

  for (j in 1:nrow(paramset.H$env)) {
    #Calculate survival time
    env <- paramset.H$env[j,]
    env.df  <- BuildEnv(temp = c(env$Ta,env$Ta), pct.rh = c(env$pct.rh,env$pct.rh), range.res.temp = 1, range.res.rh = 1, twinter = 10, winter.res = 1)
    res.surv = DynamicEnergyPd(env = env.df, bat.params = paramset[i,], fung.params = paramset[i,])

    #Subset results to single values
    res.surv = max(res.surv$time[res.surv$surv.inf == 1])

    #Calculate fungal growth rate over an hour (for use in other 2 functions)
    area = FungalGrowthRate(Tb = paramset.H$env[j,1], fung.params = as.list(paramset[i,]), t.min = 0)*ScaleFungalGrowthRate(pct.rh = paramset.H$env[j,2], fung.params = as.list(paramset[i,]))

    #Calculate EWL over a day
    res.ewl = CalcEWL(Ta=paramset.H$env[j,1], pct.rh=paramset.H$env[j,2], areaPd = area*24,  t=24, mod.params = as.list(paramset[i,]), torpid = TRUE, WNS = TRUE)$TotalEWL

    #Calculate torpor duration at end of winter (time for Pd growth is max survival time from res.surv)
    res.tbd = CalcTorporTimePd(Ta=paramset.H$env[j,1], pct.rh=paramset.H$env[j,2], WNS = TRUE, mod.params = as.list(paramset[i,]), areaPd = area*res.surv)

    #Fill in results matrix
    res_out.surv[j,] <- res.surv
    res_out.ewl[j,]  <- res.ewl
    res_out.tbd[j,]  <- res.tbd
  }

  sen.results.surv[i,] <-as.vector(t(res_out.surv))
  sen.results.ewl[i,]  <-as.vector(t(res_out.ewl))
  sen.results.tbd[i,]  <-as.vector(t(res_out.tbd))
}
save(sen.results.surv, file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_surv.RData")
save(sen.results.ewl,  file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_ewl.RData")
save(sen.results.tbd,  file = "C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_tor.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_surv.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_ewl.RData")
load("C:/Users/Katie Haase/Desktop/R Code/EnergeticModel/Output Data/sen_tor.RData")

#Apply function to new model results
PRCCresults.surv <- prcc(par.mat = paramset[,-9], model.output = sen.results.surv[,-9], routine = "blower", par.names = colnames(paramset[,-9]),output.names = seq(1,ncol(sen.results.surv[,-9])))
PRCCresults.ewl  <- prcc(par.mat = paramset[,-9], model.output = sen.results.ewl[,-9],  routine = "blower", par.names = colnames(paramset[,-9]),output.names = seq(1,ncol(sen.results.ewl[,-9])))
PRCCresults.tbd  <- prcc(par.mat = paramset[,-9], model.output = sen.results.tbd[,-9],  routine = "blower", par.names = colnames(paramset[,-9]),output.names = seq(1,ncol(sen.results.tbd[,-9])))

#Reorganize results
df.surv <- PRCCresults.surv[[1]][1]
df.ewl  <- PRCCresults.ewl[[1]][1]
df.tbd  <- PRCCresults.tbd[[1]][1]
for(x in 2:length(PRCCresults.surv)){      #Change to # in prcc results list
  df.surv <- cbind(df.a,PRCCresults.surv[[x]][1])
  df.ewl  <- cbind(df.b,PRCCresults.ewl[[x]][1])
  df.tbd  <- cbind(df.c,PRCCresults.tbd[[x]][1])
}

#Take mean prcc
means.surv <- data.frame(rowMeans(df.surv))
means.ewl  <- data.frame(rowMeans(df.ewl))
means.tbd  <- data.frame(rowMeans(df.tbd))

#Reorganize df for plotting purposes (ie remove variables that aren't within actual function)
means.surv <- data.frame(means=PRCCresults.surv[[1]][,1])
means.ewl  <- data.frame(means=PRCCresults.ewl[[1]][c(1,3,10,14:17,23:27),1])
means.tbd  <- data.frame(means=PRCCresults.tbd[[1]][c(1,3:6,10,14:19,23:27),1])

# means.ewl <- data.frame(means=PRCCresults.ewl[[1]][c(1,3,9,13:16,22:26),1])
# means.tbd <- data.frame(means=PRCCresults.tbd[[1]][c(1,3:6,9,13:18,22:26),1])

#Plot mean with significance
op <- par(family = "serif")
names.surv = expression("Body Mass","RMR","TMR"[min],"T"[eu],"T"[lc],"T"[tormin],"C"[eu],"C"[tor],"t"[tormax],"t"[eu],"Warming Rate","Cooling Rate","Rate of EWL",
                     "TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd","EWL Threshold"[infected],"EWL Threshold"[healthy],"% Euthermia Spent Flying","% Body Mass at Lean","% Body Mass as Fat",beta[1],beta[2],beta[3],mu[1],mu[2])
names.ewl = expression("Body Mass","TMR"[min],"t"[tormax],"Rate of EWL","TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd",beta[1],beta[2],beta[3],mu[1],mu[2])
names.tbd = expression("Body Mass","TMR"[min],"T"[eu],"T"[lc],"T"[tormin],"t"[tormax],"Rate of EWL","TMR increase due to Pd","Total EWL increase due to Pd growth","Rate of EWL increase due to Pd","EWL Threshold"[infected],"EWL Threshold"[healthy],beta[1],beta[2],beta[3],mu[1],mu[2])

par(mar=c(5.1,15,4.1,2.1))
barplot(means.surv[,1], xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
        names.arg= names.surv, horiz=TRUE, col = "grey85", main = "Survival")
t.cutoff=qt(0.05/2,df=100-2)
sig.cutoffs=c( (-(t.cutoff^2)-sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)),
               (-(t.cutoff^2)+sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)))
abline(v=sig.cutoffs,lty=2,col="red")
abline(v=0,lty=1,col="black")

barplot(means.ewl[,1], xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
        names.arg= names.ewl, horiz=TRUE, col = "grey85", main = "Total Evaporative Water Loss")
t.cutoff=qt(0.05/2,df=100-2)
sig.cutoffs=c( (-(t.cutoff^2)-sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)),
               (-(t.cutoff^2)+sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)))
abline(v=sig.cutoffs,lty=2,col="red")
abline(v=0,lty=1,col="black")

barplot(means.tbd[,1], xlim = c(-1,1),las=1,xlab="PRCC",cex.lab=1.5,
        names.arg= names.tbd, horiz=TRUE, col = "grey85", main = "Torpor Bout Duration")
t.cutoff=qt(0.05/2,df=100-2)
sig.cutoffs=c( (-(t.cutoff^2)-sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)),
               (-(t.cutoff^2)+sqrt((t.cutoff^4) + 4*(100-2)*(t.cutoff^2)))/(2*(100-2)))
abline(v=sig.cutoffs,lty=2,col="red")
abline(v=0,lty=1,col="black")

################################################################################
## prcc - function to calculate partial rank correlation coefficients between
##           each of p parameters and k model outputs using n different
##           observations (number of parameter sets)
##
##         Written by: Michael Buhnerkempe
##         Date: Oct. 7, 2011
##
##         Arguments:
##           par.mat = n x p matrix containing the parameter values obtained
##                        from Latin Hypercube Sampling
##           model.output = n x k matrix containing the model outputs
##           routine = how should the PRCCs be calculated? One of:
##                        "blower" - calculated according to Appendix A in
##                                   Blower and Dowlatabadi (1994). DEFAULT.
##                        "regression" - calculated using a regression approach.
##                                   Here, the partial correlation coefficient
##                                   is defined as the correlation between the
##                                   residuals after regressing a model output
##                                   on all of the parameters except for the
##                                   parameter of interest and the residuals
##                                   after regressing the parameter of interest
##                                   on all of the other parameters. This can
##                                   be interpreted as the correlation between
##                                   of a parameter and the model output when
##                                   the effects of all other parameters have
##                                   been removed.
##         par.names = names of parameters
##         output.names = names of model outputs
##               ... = additional arguments to be passed to functions called
##                     within this function
##
##
##       Output attributes:
##         $par.matrix = original matrix of parameter set
##         $model.output = original model output
##         $(model output name) = prcc results for the named model output
################################################################################

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
