#Downloading SST data
###Bash Script


### Connect to the cluster
cd /projectnb/dietzelab/kiwheel/coralReef #where the SST files are saved
qrsh #request interactive batch job

module load R
R
library(ncdf4)
setwd("/projectnb/dietzelab/kiwheel/coralReef")

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
        SSTvals <- c(SSTvals,SST[lat,long,t])
        dateVals <- c(dateVals,(paste(years[y],"-",mth,"-",dy," ",timeSeqs[t],sep="")))
      }
      nc_close(WHOIdat)
    }
  }
  return(cbind(dateVals,SSTvals))
}  

#Change file name, lat, long, and years to what you need
DT <- processWHOI_SST(1988:2003,lat=24.64745,long=-82.8881,"/projectnb/dietzelab/kiwheel/coralReef")

write.csv(DT, "/projectnb/bi594/chrislr/Dry_Tortugas.csv") #change directory where you want the file saved

#From a local terminal, cd to wherever you want the file saved on your computer and then run the following:
scp chrislr@scc1.bu.edu:/projectnb/bi594/chrislr/Dry_Tortugas.csv .