source("scrapeCoral.R")
source("plotCoralBleachingData.R")

FLdat <- scrapeCoral()
plotCoralBleachingData(FLdat=FLdat)
