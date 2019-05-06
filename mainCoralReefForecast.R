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
source("plotCalibrationFits.R")
source("SST_to_CSV.R")
source("SensitivityAnalysis.R")
source("UncertaintyAnalysisAllOut.R")
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

##1. Download the sea surface temperature (SST) data
##Note: these are already downloaded on BU's secure computing cluster at /projectnb/dietzelab/kiwheel/coralReef/
# n.cores <- 10
# registerDoParallel(cores=n.cores)
# 
# output <- foreach(y=2004:2016)%dopar%{ ##Done in parallel (could change to a for loop to remove parallel)
#   getWHOI_SST(years=y)
# }

##2. Create csv for the desired sites
##Note: these should already be included in the github repo
# SST_to_CSV.R()

##WHOI Sea Surface Temperature Data (Note: this takes a long time to run)
#dataDirectory <- "/projectnb/dietzelab/kiwheel/coralReef" ##This is a data directory on BU's scc
# SST_fileName <- "WHOI_SST_Looe_Key.csv"
# if(!file.exists){
#   SSTdat <- getWHOI_SST(years=seq(1988,2018,1))
#   write.table(SSTdat,file=SST_fileName,sep=",",row.names = FALSE,col.names = FALSE)
# }
# SSTdat <- read.csv(SST_fileName,header=FALSE)

#plotWHOI_SST(SSTdat = SSTdat)

##3. Creating Calibration Data Object
years <- seq(1988,2003) #The years for the forecast calibration
data <- list()
data$y <- donner_regions_continuous(years=years)
data$S <- Heat_Stress_Term(years=years)
data <- fillMissingBleachingData(dat=data)
data$nt <- ncol(data$S)
data$nr <- nrow(data$y)

##4. Plot Bleaching Data
plot(x=c(),y=c(),pch=20,ylim=c(0,1),xlim=range(years),xlab="Year",ylab="Bleaching Percent")

lines(years,data$y[1,],col="black",lwd=2)
lines(years,data$y[2,],col="red",lwd=2)
lines(years,data$y[3,],col="blue",lwd=2)
lines(years,data$y[4,],col="brown",lwd=2)
lines(years,data$y[5,],col="cyan",lwd=2)
legend("bottomleft",lwd=c(2,2,2,2,2),col=c("black","red","blue","brown","cyan"),c("Lower Keys","Middle Keys","Upper Keys","Biscayne Bay","Dry Tortugas"))

##5. Create Calibration Fit
calFitFile <- "HistoricalFit_continuous_simulatedBleachingData.R"
if(!file.exists(calFitFile)){
  ##Creating Model
  j.model <- createCoralForecastModelContinuous(data=data,nchain = 5)
  
  ##Running Model until convergence
  varOut <- runForecastIter(j.model=j.model,variableNames =c("beta0","beta1","tau_reg","tau_yr", "tau_proc", "x", "reg","tau_obs"),iterSize = 20000,baseNum = 10000)
  plot(varOut$params)
  plot(varOut$predict) #Diagnostic Plots
  
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

out.mat=as.data.frame(as.matrix(varOut$param)) #Parameters
out.mat2=as.data.frame(as.matrix(varOut$predict)) #Latent state variables (x)

##6. Plot Calibration Fit
plotCalibrationFits(out.mat=out.mat2,dat=data,years=years)

##7. Uncertainty Analysis:
years <- seq(1988,2016) 
S <- Heat_Stress_Term(years=years)
param.ci <- uncertainty_anal(out.mat, out.mat2, S, Nmc=5000)

plot_uncertainty(param.ci, out.mat2)

##8. Sensitivity Analysis:
param.all <- uncertainty_allout(out.mat, out.mat2, S=S, Nmc=5000)
SensitivityPlot(param.all)

##9. Forecast with Ensemble Kalman Filter
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

x <- ForecastCoralUncertModel(IC=x,beta0=calEnsembles$beta0,beta1=calEnsembles$beta1,
                              reg=calEnsembles$reg,year=yr,rec=rec,Q=calEnsembles$Q,
                              n=calEnsembles$n,S=S,nt=length(allForYears)) ##First forecast into the future

x <- t(x[,1,]) #Transpose to make dimensions work

##Forecast over the future years
for(i in 1:(length(allForYears)-1)){
  print(allForYears[i])
  jpeg(paste("coralForecast_EnKF_",allForYears[i],".jpeg",sep=""),height=6,width=10, units = 'in', res = 500)
  par(mfrow=c(2,3))

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
  
  ##Plotting the analyzed data with uncertainties and the forecasted data 
  for(r in 1:5){
    ci <- apply(x[r,,],1,quantile,c(0.025,0.5,0.975))
    plot(x=c(),y=c(),ylim=c(0,1),xlim=range(allForYears),main=allForYears[i],ylab="Percent Bleaching",xlab="Year")
    ciEnvelope(allForYears[1:i],mu.ci1[r,1:i],mu.ci3[r,1:i],col="yellow")
    lines(allForYears,mu.ci2[r,])
    ciEnvelope(forYears,ci[1,],ci[3,],col="lightblue")
    lines(forYears,ci[2,])
  }

  x <- t(x[,1,]) 
  dev.off()
}


