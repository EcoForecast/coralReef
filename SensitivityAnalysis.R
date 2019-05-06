##' @param param.all is output from uncertainty analysis

SensitivityPlot <- function(param.all) {
  
  t2=2004:2016
  loc=c("Lower Keys","Middle Keys","Upper Keys","Biscayne Bay","Dry Tortugas")
  
  N.I = as.array(param.all[[2]])
  N.IP = as.array(param.all[[3]])
  N.IPD = as.array(param.all[[4]])
  N.IPDE = as.array(param.all[[5]])
  N.IPDEA = as.array(param.all[[6]])
  
  for (r in 1:5) {
    varI     <- apply(N.I[r,,],1,var)
    varIP    <- apply(N.IP[r,,],1,var)
    varIPD   <- apply(N.IPD[r,,],1,var)
    varIPDE  <- apply(N.IPDE[r,,],1,var)
    varIPDEA <- apply(N.IPDEA[r,,],1,var)
    varMat   <- rbind(varI,varIP,varIPD,varIPDE,varIPDEA)
    
    V.pred.rel <- apply(varMat,2,function(x) {x/max(x)})
    V.pred.rel[5,] = 1
    plot(t2,V.pred.rel[1,],ylim=c(0,1),type='n',
         ylab="Proportion of Variance",xlab="time", main=(paste(loc[r], "relative variance", sep=" ")))
    ciEnvelope(t2,rep(0,ncol(V.pred.rel)),V.pred.rel[1,],col="black")
    ciEnvelope(t2,V.pred.rel[1,],V.pred.rel[2,],col="red")
    ciEnvelope(t2,V.pred.rel[2,],V.pred.rel[3,],col="green")
    ciEnvelope(t2,V.pred.rel[3,],V.pred.rel[4,],col="blue")
    ciEnvelope(t2,V.pred.rel[4,],V.pred.rel[5,],col="orange")
    #legend("topleft",legend=c("RandomEffect","Process","Driver","Parameter","InitCond"),
    #       col=c("orange", "blue", "green", "red", "black"),lty=1,lwd=5, xpd=FALSE)
  }
}
