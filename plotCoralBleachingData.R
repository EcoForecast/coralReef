##' Plots the coral bleaching data for Florida and Looe Key
##'
##' @param FLdat Coral bleaching data from simon donner database
##' @export
plotCoralBleachingData <- function(FLdat){
  years <- seq(min(FLdat$YEAR,na.rm=TRUE),max(FLdat$YEAR,na.rm=TRUE))
  yearMeans <- numeric()
  yearSD <- numeric()
  for(y in 1:length(years)){
    subDat <- FLdat[FLdat$YEAR==years[y],]
    yearMeans <- c(yearMeans,mean(subDat$SEVERITY_CODE))
    yearSD <- c(yearSD,sqrt(var(subDat$SEVERITY_CODE)))
  }
  
  plot(years,yearMeans,pch=20,ylab="Severity Code",xlab="Year",main="Severity of Coral Bleaching in Florida")
  for(i in 1:length(yearSD)){
    if(!is.na(yearSD[i])){
      Q1 <- yearMeans[i]-yearSD[i]
      Q3 <- yearMeans[i]+yearSD[i]
      CI <- seq(Q1,Q3,0.1)
      lines(rep(years[i],length(CI)),CI,col="gray")
    }
  }
  
  ##Plot Data for one site: Looe Key
  LKdat <- rbind(FLdat[FLdat$LOCATION=="Looe Key",],FLdat[FLdat$LOCATION=="Looe Key, Florida",],FLdat[FLdat$LOCATION=="Looe Key Reef",])
  LKdat <- LKdat[!is.na(LKdat$COUNTRY),]
  plot(LKdat$YEAR,LKdat$SEVERITY_CODE,pch=20,ylab="Severity Code",xlab="Year",main="Severity of Coral Bleaching in Looe Key, FL")
}