##' @export

source("scrapeCoral.R")
source("HughesData.R")


donner_regions <- function() {y
#Coral bleaching data
FLdat <- scrapeCoral()
Hughes <- HughesData()
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
BiscayneBay1=FLdat.short[c(UKcols),]


#Function to consolidate regional datasets and add Hughes data
RegionalMeans=function(df) {
  x=c()
  sev=c()
  y=unique(df$YEAR)
  for (a in 1:length(y)) {
    b=df$SEVERITY_CODE[(df$YEAR==y[a])]
    x=c(x, b)
    sev=c(sev, mean(x))
  }
  
  df1=data.frame(LATITUDE=mean(df$LATITUDE), LONGITUDE=mean(df$LONGITUDE), YEAR=unique(df$YEAR), SEVERITY_CODE=sev)
  #Removing duplicated years from Hughes
  hix=c()
  for (a in 1:length(y)) {
    ix=Hughes$YEAR==y[a]
    hix=c(hix, which(ix==TRUE))
  }
  Hughes1=Hughes[-c(hix),]
  #Binding reduced Hughes dataset with aggregated regional data
   df2 = rbind(df1, Hughes1)
  
  return(df2)
}
#Runs function for each region
DryTortugas=RegionalMeans(DryTortugas1)
LowerKeys=RegionalMeans(LowerKeys1)
MiddleKeys=RegionalMeans(MiddleKeys1)
UpperKeys=RegionalMeans(UpperKeys1)
BiscayneBay=RegionalMeans(BiscayneBay1)

return(DryTortugas, LowerKeys, MiddleKeys, UpperKeys, BiscayneBay)
}
