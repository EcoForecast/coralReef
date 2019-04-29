


#Pass in array of N.ci's, have for loop go over ncol or whatever its called
plot_uncertainty <- function(param.ci, out.mat2) {
t1=1988:2003
t2=2004:2016
t=1988:2016
trans=1 #transparency

N.det = as.array(param.ci[[1]])
N.I.ci = as.array(param.ci[[2]])
N.IP.ci = as.array(param.ci[[3]])
N.IPD.ci = as.array(param.ci[[4]])
N.IPDE.ci = as.array(param.ci[[5]])
N.IPDEA.ci = as.array(param.ci[[6]])

loc=c("Lower Keys","Middle Keys","Upper Keys","Biscayne Bay","Dry Tortugas")
for(r in 1:5) {
  col=seq(0, 79, by=5)
  col=col+r
  bp=out.mat2[,col]
  bp.ci=apply(bp, 2,quantile,c(0.025,0.5,0.975))
  plot(t1, bp.ci[2,], xlim=c(1988, 2016), ylim=c(0, 1.1),
       xlab="year", ylab="percent bleaching", main=(paste(loc[r], "Uncertainty", sep=" ")))
  ecoforecastR::ciEnvelope(t1,bp.ci[1,],bp.ci[3,],col=col.alpha("lightBlue", .6))
  lines(t1, bp.ci[2,], col="blue")
  ecoforecastR::ciEnvelope(t2,N.IPDEA.ci[r,,1],N.IPDEA.ci[r,,3],col=col.alpha("orange",trans))
  ecoforecastR::ciEnvelope(t2,N.IPDE.ci[r,,1],N.IPDE.ci[r,,3],col=col.alpha("blue",trans))
  ecoforecastR::ciEnvelope(t2,N.IPD.ci[r,,1],N.IPD.ci[r,,3],col=col.alpha("green",trans))
  ecoforecastR::ciEnvelope(t2,N.IP.ci[r,,1],N.IP.ci[r,,3],col=col.alpha("red",trans))
  ecoforecastR::ciEnvelope(t2,N.I.ci[r,,1],N.I.ci[r,,3],col=col.alpha("black",trans))
  lines(t2,N.det[r,,1],lwd=0.5, col="purple")
  legend("topleft", legend=c("IC", "Param", "Driver", "Process", "Random"), 
         fill=c('black','red','green','blue','orange'))
  #Add legend
}
}
#add regions title (figure out which is which)