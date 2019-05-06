##' Plots the calibration fits
##'
##' @param out.mat JAGS output in matrix form
##' @export
plotCalibrationFits <- function(out.mat,dat,years){
  regions <- c("Lower Keys","Middle Keys","Upper Keys","Biscayne Bay","Dry Tortugas")
  for(r in 1:5){
    xCIs <- matrix(nrow=3,ncol=0)
    for(j in 1:(ncol(out.mat)/5)){
      colName <- paste("x[",r,",",j,"]",sep="")
      x <- out.mat[,colName]
      xCIs <- cbind(xCIs,quantile(x,c(0.025,0.5,0.975)))
    }
    plot(years,dat$y[r,],pch=20,ylab="Percent Bleached",xlab="Time (year)",main=regions[r])

    ciEnvelope(years,xCIs[1,],xCIs[3,],col="lightblue")
    lines(years,xCIs[2,])
    points(years,dat$y[r,],pch=20)
    
  }
  
}