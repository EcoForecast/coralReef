##' @export

source("scrapeCoral.R")
source("HughesData.R")


donner_regions <- function() {
#Coral bleaching data
FLdat <- scrapeCoral()
Hughes <- HughesData()
#Remove unneccessary columns; qualitative or NA values
FLdat.short= FLdat[c(2,4:11)]

#Merging regions from donner dataset by name
#May go back and check/ change this based on LAT/LONG distances
donner_regions <- list(FLdat.short[FLdat.short$LOCATION==IDs[1],])

UnIDs<-unique(FLdat.short$LOCATION)
for (i in 2:length(IDs)){ 
  temp <- data.frame(FLdat.short[FLdat.short$LOCATION==UnIDs[i],])
  donner_regions[[i]]= temp
}

#The location closest to the Hughes dataset coordinates was "Middle Keys; 11 ft mound". 
#The Hughes data will be aggregated to that site

#Finds the index to insert the Hughes dataset in
for (ix in 1:length(donner_regions)) {
  if(donner_regions[[ix]]$LOCATION=="Middle Keys: 11 ft. mound") {
    break
  }
}

#Add the Hughes data to the middle keys 11 ft mound df in the donner data list
temp <- data.frame(donner_regions[[ix]])
temp = rbind(temp, Hughes)#, by.y =c("YEAR", "SEVERITY_CODE"))
donner_regions[[ix]]= temp



return(donner_regions)
}
