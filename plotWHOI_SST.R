##' Plots the sea surface temperature from WHOI
##'
##' @param SSTdat matrix with two columns (date/time and SST)
##' @export
plotWHOI_SST <- function(SSTdat){
  plot(as.POSIXlt(SSTdat[,1]),SSTdat[,2],pch=20,main="Sea Surface Temperature at Looe Key",ylab="SST (C)",xlab="Time")
}