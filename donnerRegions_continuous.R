##Function to compute the bleaching data for each region
##' 
##' @param years The desired years
##' @export
donner_regions_continuous <- function(years) {
  #source("scrapeCoral.R")
  #source("HughesData.R")
  #source("RegionalMeans.R")
  #Coral bleaching data
  FLdat <- scrapeCoral()
  Hughes <- HughesData_continuous()
  #Remove unneccessary columns; qualitative or NA values
  FLdat.short= FLdat[c(2,4, 5, 7, 9)]
  
  #Remove Pacific Islands location that was mislabeled 
  PIx=grep("Palmyra", FLdat.short$LOCATION)
  FLdat.short=FLdat.short[-c(PIx),]
  
  #Separating into 5 regions by lat/long groupings. Regions are according to Wagner et al. (2010)
  #Note that some locations labelled upper or middle keys are mixed into the same region - this is based off coordinates, which seem a better determinant for a spatial model
  #Note that Biscayne Bay includes areas further North than in the figure in Wagner et al. 
  #
  DTcols=c(grep("-82.7", FLdat.short$LONGITUDE), grep("-82.8", FLdat.short$LONGITUDE), grep("-82.9", FLdat.short$LONGITUDE))
  DryTortugas1=FLdat.short[c(DTcols),]
  #Removing point with high latitude
  DryTortugas1=DryTortugas1[-c(grep("27", DryTortugas1$LATITUDE)),]
  
  LKcols=c(grep("-81.19", FLdat.short$LONGITUDE), grep("-81.3", FLdat.short$LONGITUDE), grep("-81.4", FLdat.short$LONGITUDE), grep("-81.5", FLdat.short$LONGITUDE), grep("-81.6", FLdat.short$LONGITUDE), grep("-81.7", FLdat.short$LONGITUDE), grep("-81.8", FLdat.short$LONGITUDE), grep("-82.0", FLdat.short$LONGITUDE))
  LowerKeys1=FLdat.short[c(LKcols),]
  
  MKcols=c(grep("-81.11", FLdat.short$LONGITUDE), grep("-81.0", FLdat.short$LONGITUDE), grep("-80.9", FLdat.short$LONGITUDE), grep("-80.8", FLdat.short$LONGITUDE), grep("-80.7", FLdat.short$LONGITUDE), grep("-80.6", FLdat.short$LONGITUDE))
  MiddleKeys1=FLdat.short[c(MKcols),]
  
  UKcols=c(grep("24.93", FLdat.short$LATITUDE), grep("24.94", FLdat.short$LATITUDE), grep("24.98", FLdat.short$LATITUDE), grep("25.0", FLdat.short$LATITUDE), grep("-25.1", FLdat.short$LATITUDE), grep("-25.2", FLdat.short$LATITUDE))
  UpperKeys1=FLdat.short[c(UKcols),]
  
  BBcols=c(grep("25.3", FLdat.short$LATITUDE), grep("25.4", FLdat.short$LATITUDE), grep("25.5", FLdat.short$LATITUDE), grep("26", FLdat.short$LATITUDE))
  BiscayneBay1=FLdat.short[c(BBcols),]
  
  #Runs function for each region
  DryTortugas=RegionalMeans_continuous(DryTortugas1)
  LowerKeys=RegionalMeans_continuous(LowerKeys1)
  MiddleKeys=RegionalMeans_continuous(MiddleKeys1)
  UpperKeys=RegionalMeans_continuous(UpperKeys1)
  BiscayneBay=RegionalMeans_continuous(BiscayneBay1)
  
  #Insert function that changes discrete 0:2 data to percentage bleaching
  #Hughes data --> 0 no bleaching, M 1-30%, S >30%
  #Donner data --> -1 unknown, 0 no bleaching, 1 1-10%, 2 10-50%, 3 >50%
  #rnorm Hughes and donner datas separately, then combine
  
  output <- numeric()
  for(y in 1:length(years)){
    #
    currentDat <- c(LowerKeys[LowerKeys$YEAR==years[y],]$SEVERITY_CODE,
                    MiddleKeys[MiddleKeys$YEAR==years[y],]$SEVERITY_CODE,
                    UpperKeys[UpperKeys$YEAR==years[y],]$SEVERITY_CODE,
                    BiscayneBay[BiscayneBay$YEAR==years[y],]$SEVERITY_CODE,
                    DryTortugas[DryTortugas$YEAR==years[y],]$SEVERITY_CODE)
    output <- cbind(output,currentDat)
  }
  colnames(output) <- years
  
  return(output)
}
