source("scrapeCoral.R")
source("plotCoralBleachingData.R")
source("getWHOI_SST.R")
source("plotWHOI_SST.R")
source("createCoralForecastModel.R")
source("createCoralForecastModelContinuous.R")
source("donnerRegions.R")
source("donnerRegions_continuous.R")
source("RegionalMeans.R")
source("RegionalMeans_Continuous.R")
source("HughesData_Continuous.R")
source("Heat_Stress_Term.R")
source("fillMissingBleachingData.R")
source("HughesData.R")
source("runForecastIter.R")
source("forecastCoralUncertModel.R")
source("UncertaintyAnalysis.R")
source("UncertaintyPlot.R")
source("createCalEnsemble.R")
source("KalmanAnalysis.R")
library("ncdf4")
library("RCurl")
library("XML")
library("devtools")
library("rjags")
library("runjags")
library("ecoforecastR")
library(tidyr)
library(dplyr)
library(reshape2)
library(zoo)
library(mvtnorm)

years <- seq(1988,2003) #The years for the forecast calibration
data <- list()
##Coral Bleaching Data
#FLdat <- scrapeCoral()
#plotCoralBleachingData(FLdat=FLdat)
data$y <- donner_regions_continuous(years=years)
#data$b <- data$b + 1 ##Done to set categories as 1,2,3 for the model

##WHOI Sea Surface Temperature Data (Note: this takes a long time to run)
#dataDirectory <- "/projectnb/dietzelab/kiwheel/coralReef" ##This is a data directory on BU's scc
# SST_fileName <- "WHOI_SST_Looe_Key.csv"
# if(!file.exists){
#   SSTdat <- getWHOI_SST(years=seq(1988,2018,1))
#   write.table(SSTdat,file=SST_fileName,sep=",",row.names = FALSE,col.names = FALSE)
# }
# SSTdat <- read.csv(SST_fileName,header=FALSE)

#plotWHOI_SST(SSTdat = SSTdat)

##Need code that created the csv files of SST
#data$b2 <- donner_regions(years=years)

data$S <- Heat_Stress_Term(years=years)
data <- fillMissingBleachingData(dat=data)


plot(x=c(),y=c(),pch=20,ylim=c(0,1),xlim=range(years),xlab="Year",ylab="Bleaching Percent")
#lines(data$S[1,],data$x[1,])

lines(years,data$y[2,],col="red")
lines(years,data$y[3,],col="blue")
lines(years,data$y[4,],col="brown")
lines(years,data$y[5,],col="cyan")

plot(x=data$S,y=data$y,pch=20,ylim=c(0,1),xlim=c(0,20),ylab="Bleaching Percent",xlab="Heat Stress")
#lines(data$S[1,],data$x[1,])


##Simulating bleached data:
# data$b <- round(data$S/4,digits=0)
# data$b[data$b==0] <- 1
# plot(data$S,data$b,pch=20)
# abline(lm(data$b~data$S))

data$nt <- ncol(data$S)
data$nr <- nrow(data$y)
# inits.mu <- createInits(data=data,PFT="DB")
xs <- numeric()
ys <- numeric()
for(i in 1:5){
  xs <- c(xs,data$S[i,])
  ys <- c(ys,data$y[i,])
}
abline(lm(ys~xs),col="red")

calFitFile <- "HistoricalFit_continuous_simulatedBleachingData.R"
if(!file.exists(calFitFile)){
  ##Creating Model
  j.model <- createCoralForecastModelContinuous(data=data,nchain = 5)
  
  ##Running Model until convergence
  #varOut <- coda.samples(model=j.model,variable.names =c("beta0","beta1","tau_reg","tau_yr","x"), n.iter=1000)
  varOut <- runForecastIter(j.model=j.model,variableNames =c("beta0","beta1","tau_reg","tau_yr", "tau_proc", "x", "reg","tau_obs"),iterSize = 10000,baseNum = 5000)
  #summary(varOut$params)#,"tau_proc","year","reg","rec")
  
  
  #out.mat <- as.matrix(varOut)
  plot(varOut$params)
  plot(varOut$predict)
  
  ##Thin Data:
  
  out.mat <- as.matrix(varOut$params)
  thinAmount <- round(nrow(out.mat)/5000,digits=0)
  varOut2 <- list()
  varOut2$param <- window(varOut$param,thin=thinAmount)
  varOut2$predict <- window(varOut$predict,thin=thinAmount)
  varOut <- varOut2
  
  ##Save historical fit
  save(varOut,file=calFitFile)
}

load(calFitFile)


out.mat=as.data.frame(as.matrix(varOut$param))
out.mat2=as.data.frame(as.matrix(varOut$predict))

##Uncertainty Analysis:
years <- seq(1988,2016) 
S <- Heat_Stress_Term(years=years)
param.ci <- uncertainty_anal(out.mat, out.mat2, S, Nmc=5000)

plot_uncertainty(param.ci, out.mat2)

##Forecast with Ensemble Kalman Filter
allForYears <- seq(2004,2016)
Nmc <- 1000
calEnsembles <- createCalEnsemble(out.mat=out.mat,Nmc=Nmc) #Create ensembles based off of samples from the posteriors

S <- array(dim=c(5,length(allForYears),Nmc))
rec <- array(dim=c(5,length(allForYears),Nmc))
yr <- array(dim=c(5,length(allForYears),Nmc))
for(i in 1:length(allForYears)){ ##Creates the rest of the parameters of the ensembles
  print(allForYears[i])
  S[,i,] <- rep(Heat_Stress_Term(years=allForYears[i]),Nmc)
  rec[,i,] <- calEnsembles$rec[,1,]
  yr[,i,] <- calEnsembles$year[,1,]
}
mu.ci1 <- matrix(nrow=5,ncol=length(allForYears)) ##Storage for the analyzed data
mu.ci2 <- matrix(nrow=5,ncol=length(allForYears))
mu.ci3 <- matrix(nrow=5,ncol=length(allForYears))

x <- calEnsembles$ICs #The first analyzed state is the last latent state from the calibration
##Initial forecast
print(dim(x))
x <- ForecastCoralUncertModel(IC=x,beta0=calEnsembles$beta0,beta1=calEnsembles$beta1,
                              reg=calEnsembles$reg,year=yr,rec=rec,Q=calEnsembles$Q,
                              n=calEnsembles$n,S=S,nt=length(allForYears)) ##First forecast into the future
x <- t(x[,1,]) #Transpose to make dimensions work
jpeg("coralEnKF.jpeg",height=40,width=15, units = 'in', res = 1000)
par(mfrow=c(length(allForYears),5))
##Forecast over the future years
for(i in 1:(length(allForYears)-1)){
  print(allForYears[i])
  forYears <- allForYears[i:length(allForYears)] #Remaining years to forecast
  ##Create input objects for the Kalman Analysis
  mu.f <- colMeans(x)
  print(mu.f)
  P.f <- cov(x)
  subDat <- list()
  subDat$y <- donner_regions_continuous(years=allForYears[i])
  subDat$S <- Heat_Stress_Term(years=allForYears[i])
  y<- fillMissingBleachingData(dat=subDat)$y
  R <- diag(mean(out.mat$tau_obs),5,5)
  H <- diag(1,5,5)
  KA <- KalmanAnalysis(mu.f=mu.f,P.f=P.f,Y=y,R=R,H=H) ##Kalman Analysis

  x = rmvnorm(Nmc,KA$mu.a,KA$P.a) ##Sample from a multivariate norm distribution that has the characteristics solved for in the KA
  mu.ci1[,i] <- apply(x,2,quantile,c(0.025)) ##Save analyzed latent states of bleaching (quantiles)
  mu.ci2[,i] <- apply(x,2,quantile,c(0.5))
  mu.ci3[,i] <- apply(x,2,quantile,c(0.975))
  
  ##Next forecast
  x <- t(x)

  x <- ForecastCoralUncertModel(IC=x,beta0=calEnsembles$beta0,beta1=calEnsembles$beta1,
                                reg=calEnsembles$reg,year=yr[,i:length(allForYears),],
                                rec=rec[,i:length(allForYears),],Q=calEnsembles$Q,n=Nmc,S=S,nt=length(forYears))
  

  
  ##Plotting the analyzed data with uncertainties and the forecasted data (currently just done for one region)
  for(r in 1:5){
    ci <- apply(x[r,,],1,quantile,c(0.025,0.5,0.975))
    plot(x=c(),y=c(),ylim=c(0,1),xlim=range(allForYears),main=allForYears[i],ylab="Percent Bleaching",xlab="Year")
    ciEnvelope(allForYears[1:i],mu.ci1[r,1:i],mu.ci3[r,1:i],col="yellow")
    lines(allForYears,mu.ci2[r,])
    ciEnvelope(forYears,ci[1,],ci[3,],col="lightblue")
    lines(forYears,ci[2,])
  }

  
  x <- t(x[,1,]) ##Restrict to just the first forecasted year

}
dev.off()

