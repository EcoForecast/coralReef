source("scrapeCoral.R")
source("plotCoralBleachingData.R")
source("getWHOI_SST.R")
source("plotWHOI_SST.R")
source("createCoralForecastModel.R")
source("donnerRegions.R")
source("RegionalMeans.R")
library("ncdf4")
library("RCurl")
library("XML")
library("devtools")
library("rjags")

years <- seq(1988,2003) #The years for the forecast calibration
data <- list()
##Coral Bleaching Data
#FLdat <- scrapeCoral()
#plotCoralBleachingData(FLdat=FLdat)
data$b <- donner_regions(years=years)
data$b <- data$b + 1 ##Done to set categories as 1,2,3 for the model

##WHOI Sea Surface Temperature Data (Note: this takes a long time to run)
dataDirectory <- "/projectnb/dietzelab/kiwheel/coralReef" ##This is a data directory on BU's scc
# SST_fileName <- "WHOI_SST_Looe_Key.csv"
# if(!file.exists){
#   SSTdat <- getWHOI_SST(years=seq(1988,2018,1))
#   write.table(SSTdat,file=SST_fileName,sep=",",row.names = FALSE,col.names = FALSE)
# }
# SSTdat <- read.csv(SST_fileName,header=FALSE)

#plotWHOI_SST(SSTdat = SSTdat)


#data$nt <- ncol(data$S)
data$nr <- nrow(data$b)

#j.model <- createCoralForecastModel(data=data,nchain = 10)

#varOut <- coda.samples(model=j.model,variable.names = c("beta0","beta1","tau_reg","tau_yr","b","mu","lowLimit","upperLimit"),n.iter = 1000000)


#summary(varOut)

