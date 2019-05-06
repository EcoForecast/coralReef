##' Processes the SST files into csv's for each site
##'
##' @param directory The desired data directory where SST has been downloaded in and where you want the CSV's to be created in
##' @import ncdf4
##' @export
SST_to_CSV <- function(directory){
  setwd(directory)
  #Create CSVs for each site for the "observed" data
  DT <- processWHOI_SST(1988:2016,lat=24.64745,long=-82.8881,directory)
  write.csv(DT, paste(directory,"/Dry_Tortugas.csv",sep="")) #change directory where you want the file saved

  BB <- processWHOI_SST(1988:2016,lat=25.71338,long=-80.40222,directory)
  write.csv(BB, paste(directory,"/Biscayne_Bay.csv",sep=""))

  LK <- processWHOI_SST(1988:2016,lat=24.47465,long=-81.5672,directory)
  write.csv(LK, paste(directory,"/Lower_Keys.csv",sep=""))

  MK <- processWHOI_SST(1988:2016,lat=24.73725,long=-80.88384,directory)
  write.csv(MK, paste(directory,"/Middle_Keys.csv",sep=""))

  UK <- processWHOI_SST(1988:2016,lat=25.00507,long=-80.42312,directory)
  write.csv(UK, paste(directory,"/Upper_Keys.csv",sep=""))
}