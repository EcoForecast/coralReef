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

years <- seq(1988,2003) #The years for the forecast calibration
data <- list()
##Coral Bleaching Data
#FLdat <- scrapeCoral()
#plotCoralBleachingData(FLdat=FLdat)
data$x <- donner_regions_continuous(years=years)
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
data$x <- data$x[1:5,]
data$S <- Heat_Stress_Term(years=years)
data <- fillMissingBleachingData(data=data)
plot(data$S,data$x,pch=20)

##Simulating bleached data:
# data$b <- round(data$S/4,digits=0)
# data$b[data$b==0] <- 1
# plot(data$S,data$b,pch=20)
# abline(lm(data$b~data$S))

data$nt <- ncol(data$S)
data$nr <- nrow(data$x)
# inits.mu <- createInits(data=data,PFT="DB")
xs <- numeric()
ys <- numeric()
for(i in 1:5){
  xs <- c(xs,data$S[i,])
  ys <- c(ys,data$x[i,])
}
abline(lm(ys~xs),col="red")

##Creating Model
j.model <- createCoralForecastModelContinuous(data=data,nchain = 5)

##Running Model until convergence
varOut <- runForecastIter(j.model=j.model,variableNames =c("beta0","beta1","tau_reg","tau_yr","x"),iterSize = 10000,baseNum = 10000)
summary(varOut$params)

#out.mat <- as.matrix(varOut)
plot(varOut$params) 

##Save historical fit
save(varOut,file="HistoricalFit_continuous_simulatedBleachingData.R")


