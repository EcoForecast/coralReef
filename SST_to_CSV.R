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

#Create CSVs for each site for the "observed" data
DT <- processWHOI_SST(1988:2003,lat=24.64745,long=-82.8881,"/projectnb/dietzelab/kiwheel/coralReef")
write.csv(DT, "/projectnb/bi594/chrislr/Dry_Tortugas.csv") #change directory where you want the file saved

BB <- processWHOI_SST(1988:2003,lat=25.71338,long=-80.40222,"/projectnb/dietzelab/kiwheel/coralReef")
write.csv(BB, "/projectnb/bi594/chrislr/Biscayne_Bay.csv")

LK <- processWHOI_SST(1988:2003,lat=24.47465,long=-81.5672,"/projectnb/dietzelab/kiwheel/coralReef")
write.csv(LK, "/projectnb/bi594/chrislr/Lower_Keys.csv")

MK <- processWHOI_SST(1988:2003,lat=24.73725,long=-80.88384,"/projectnb/dietzelab/kiwheel/coralReef")
write.csv(MK, "/projectnb/bi594/chrislr/Middle_Keys.csv")

UK <- processWHOI_SST(1988:2003,lat=25.00507,long=-80.42312,"/projectnb/dietzelab/kiwheel/coralReef")
write.csv(UK, "/projectnb/bi594/chrislr/Upper_Keys.csv")


#Create CSVs for each site for the model validation data
DTn <- processWHOI_SST(2004:2016,lat=24.64745,long=-82.8881,"/projectnb/dietzelab/kiwheel/coralReef")
write.csv(DTn, "/projectnb/bi594/chrislr/Dry_Tortugas_new.csv") #change directory where you want the file saved

BBn <- processWHOI_SST(2004:2016,lat=25.71338,long=-80.40222,"/projectnb/dietzelab/kiwheel/coralReef")
write.csv(BBn, "/projectnb/bi594/chrislr/Biscayne_Bay_new.csv")

LKn <- processWHOI_SST(2004:2016,lat=24.47465,long=-81.5672,"/projectnb/dietzelab/kiwheel/coralReef")
write.csv(LKn, "/projectnb/bi594/chrislr/Lower_Keys_new.csv")

MKn <- processWHOI_SST(2004:2016,lat=24.73725,long=-80.88384,"/projectnb/dietzelab/kiwheel/coralReef")
write.csv(MKn, "/projectnb/bi594/chrislr/Middle_Keys_new.csv")

UKn <- processWHOI_SST(2004:2016,lat=25.00507,long=-80.42312,"/projectnb/dietzelab/kiwheel/coralReef")
write.csv(UKn, "/projectnb/bi594/chrislr/Upper_Keys_new.csv")

#From a local terminal, cd to wherever you want the file saved on your computer and then run the following:
scp chrislr@scc1.bu.edu:/projectnb/bi594/chrislr/*.csv .