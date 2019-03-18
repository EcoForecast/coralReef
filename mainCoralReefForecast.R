source("scrapeCoral.R")
source("plotCoralBleachingData.R")
source("getWHOI_SST.R")
source("plotWHOI_SST.R")

##Coral Bleaching Data
FLdat <- scrapeCoral()
plotCoralBleachingData(FLdat=FLdat)

##WHOI Sea Surface Temperature Data (Note: this takes a long time to run)
SSTdat <- getWHOI_SST(years=seq(1988,2018,1))
plotWHOI_SST(SSTdat = SSTdat)
