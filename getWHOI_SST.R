##Script to download whoi sea surface temperature
##'
##' @param years vector of years to download
##' @import ncdf4
##' @import RCurl
##' @import XML
##' @import devtools
##' @export
getWHOI_SST <- function(years){
  timeSeqs <- c("01:00:00","04:00:00","07:00:00","10:00:00","13:00:00","16:00:00","19:00:00","22:00:00")
  lat <- 24.6160 #For Looe Key
  lon <- 360-81.4000
  
  SSTvals <- numeric()
  dateVals <- character()
  
  for(y in years){
    WHOI_Folder <- paste("https://www.ncei.noaa.gov/data/sea-surface-temperature-whoi/access/",y,"/",sep="")
    WHOI_html <- getURL(WHOI_Folder)
    dat <- readHTMLTable(WHOI_html)[[1]]
    dat <- dat[!is.na(dat$Name),]
    dat <- dat[dat$Name!="Parent Directory",]
    for(f in 1:nrow(dat)){
      subFileName <- as.character(dat$Name[f])
      WHOI_fileName <- paste(WHOI_Folder,subFileName,sep="")
      system(paste("wget",WHOI_fileName))
      WHOIdat <- nc_open(subFileName)
      SST <- ncvar_get(WHOIdat,"sea_surface_temperature")
      mth <- substr(strsplit(subFileName,"_")[[1]][4],6,7)
      dy <- substr(strsplit(subFileName,"_")[[1]][4],8,9)
      for(t in 1:length(timeSeqs)){
        SSTvals <- c(SSTvals,SST[lat,lon,t])
        dateVals <- c(dateVals,paste(y,"-",mth,"-",dy," ",timeSeqs[t],sep=""))
      }
    }
  }
  return(cbind(dateVals,SSTvals))
}  