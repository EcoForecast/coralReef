##Script to process whoi sea surface temperature
##'
##' @param years vector of years to download ##Example: c(1988,2003)
##' @param lat The site latitude ##Example 24.6160
##' @param long The site longitude #Example -81.4000
##' @param dataDirectory The file path to the data files ##Example: /projectnb/dietzelab/kiwheel/coralReef
##' @import ncdf4
##' @import RCurl
##' @import XML
##' @import devtools
##' @export
processWHOI_SST <- function(years,lat,long,dataDirectory){
  long <- 360+long ##Indexed by degrees east
  timeSeqs <- c("01:00:00","04:00:00","07:00:00","10:00:00","13:00:00","16:00:00","19:00:00","22:00:00")
  
  SSTvals <- numeric()
  dateVals <- character()
  dateVals <- as.POSIXct(dateVals)
  for(y in 1:length(years)){
    print(years[y])
    files <- dir(path=dataDirectory,pattern=paste("D",years[y],sep=""))
    for(f in 1:length(files)){
      print(files[f])
      WHOIdat <- nc_open(files[f])
      SST <- ncvar_get(WHOIdat,"sea_surface_temperature")
      mth <- substr(strsplit(files[f],"_")[[1]][4],6,7)
      dy <- substr(strsplit(files[f],"_")[[1]][4],8,9)
      for(t in 1:length(timeSeqs)){
        SSTvals <- c(SSTvals,SST[lat,lon,t])
        dateVals <- c(dateVals,as.POSIXct(paste(years[y],"-",mth,"-",dy," ",timeSeqs[t],sep="")))
      }
    }
  }
  return(cbind(dateVals,SSTvals))
}  