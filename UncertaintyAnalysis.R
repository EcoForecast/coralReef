##' Returns confidence intervals with different model uncertanties for uncertainty analysis
##' @param out.mat jags parameter output matrix
##' @param out.mat2 jags prediction output matrix
##' @param S heat stress 1988 to 2016
##' @param Nmc number of ensemble members to run
##' @export 

##Reference: ForecastCoralUncertModel <- function(IC,beta0,beta1,year,reg,Q,n,S)
uncertainty_anal <- function(out.mat, out.mat2, S, Nmc){
  
  params <- as.matrix(out.mat)
  IC <- as.matrix(out.mat2)
  param.mean <-apply(params, 2, mean)
  
  prow <- sample.int(nrow(params), Nmc, replace=TRUE) #random sample of parameter values
  Qmc <- 1/sqrt(params[prow, "tau_proc"]) #Convert to SD
  
  N.det<- ForecastCoralUncertModel(IC=c(mean(IC[,"x[1,16]"]),
                                        mean(IC[,"x[2,16]"]),
                                        mean(IC[,"x[3,16]"]),
                                        mean(IC[,"x[4,16]"]),
                                        mean(IC[,"x[5,16]"])), #Mean IC in 2003, after model calibration
                                   beta0=param.mean["beta0"],
                                   beta1=param.mean["beta1"],
                                   reg=c(param.mean["reg[1]"],
                                         param.mean["reg[2]"],
                                         param.mean["reg[3]"],
                                         param.mean["reg[4]"],
                                         param.mean["reg[5]"]),
                                   tau_yr=param.mean["tau_yr"],
                                   Q=0,
                                   n=1,
                                   S=S)
N.I<- ForecastCoralUncertModel(IC=c(IC[prow, "x[1,16]"],
                                  IC[prow, "x[2,16]"], 
                                  IC[prow, "x[3,16]"], 
                                  IC[prow, "x[4,16]"], 
                                  IC[prow, "x[5,16]"]),#Initial condition spread
                               beta0=param.mean["beta0"],
                               beta1=param.mean["beta1"],
                               reg=c(param.mean["reg[1]"],
                                     param.mean["reg[2]"],
                                     param.mean["reg[3]"],
                                     param.mean["reg[4]"],
                                     param.mean["reg[5]"]),
                               tau_yr=param.mean["tau_yr"],
                               Q=0,
                               n=Nmc,
                               S=S)
N.IP<- ForecastCoralUncertModel(IC=c(IC[prow, "x[1,16]"],
                                     IC[prow, "x[2,16]"], 
                                     IC[prow, "x[3,16]"], 
                                     IC[prow, "x[4,16]"], 
                                     IC[prow, "x[5,16]"]),
                               beta0=params[prow, "beta0"], #Parameters beta0 and beta1
                               beta1=params[prow, "beta1"],
                               reg=c(params[prow, "reg[1]"], #Parameters location and year effect
                                     params[prow, "reg[2]"],
                                     params[prow, "reg[3]"],
                                     params[prow, "reg[4]"],
                                     params[prow, "reg[5]"]),
                               tau_yr=params[prow, "tau_yr"],
                               Q=0,
                               n=Nmc,
                               S=S)
S_u=S
for (r in 1:nrow(S_u)) {
  for (t in 1:ncol(S_u)) {
    S_u[r,t] <- rnorm(1, S[r,t], 1) #1 day SD assumed
    S_u[r,t] = max(S_u[r,t], 0)
  }
}
S_u

N.IPD<- ForecastCoralUncertModel(IC=c(IC[prow, "x[1,16]"],
                                      IC[prow, "x[2,16]"], 
                                      IC[prow, "x[3,16]"], 
                                      IC[prow, "x[4,16]"], 
                                      IC[prow, "x[5,16]"]),
                                 beta0=params[prow, "beta0"],
                                 beta1=params[prow, "beta1"],
                                 reg=c(params[prow, "reg[1]"],
                                       params[prow, "reg[2]"],
                                       params[prow, "reg[3]"],
                                       params[prow, "reg[4]"],
                                       params[prow, "reg[5]"]),
                                 tau_yr=params[prow, "tau_yr"],
                                 Q=0,
                                 n=Nmc,
                                 S=S_u) #Driver heat stress data

N.IPDE<- ForecastCoralUncertModel(IC=c(IC[prow, "x[1,16]"],
                                       IC[prow, "x[2,16]"], 
                                       IC[prow, "x[3,16]"], 
                                       IC[prow, "x[4,16]"], 
                                       IC[prow, "x[5,16]"]),
                                  beta0=params[prow, "beta0"],
                                  beta1=params[prow, "beta1"],
                                  reg=c(params[prow, "reg[1]"],
                                        params[prow, "reg[2]"],
                                        params[prow, "reg[3]"],
                                        params[prow, "reg[4]"],
                                        params[prow, "reg[5]"]),
                                  tau_yr=params[prow, "tau_yr"],
                                  Q=Qmc, #Process error
                                  n=Nmc,
                                  S=S_u) 

tau.mc.reg <- 1/sqrt(params[prow,"tau_reg"])
aNew.mc.reg1 <- rnorm(Nmc,0,tau.mc.reg)
aNew.mc.reg2 <- rnorm(Nmc,0,tau.mc.reg)
aNew.mc.reg3 <- rnorm(Nmc,0,tau.mc.reg)
aNew.mc.reg4 <- rnorm(Nmc,0,tau.mc.reg)
aNew.mc.reg5 <- rnorm(Nmc,0,tau.mc.reg)

tau.mc.y <- 1/sqrt(params[prow,"tau_yr"])
aNew.mc.y <- rnorm(Nmc,1/sqrt(param.mean["tau_yr"]),tau.mc.y)
for (a in 1:length(aNew.mc.y)){
  aNew.mc.y[a] = max(aNew.mc.y[a], 0)
}

N.IPDEA<- ForecastCoralUncertModel(IC=c(IC[prow, "x[1,16]"],
                                        IC[prow, "x[2,16]"], 
                                        IC[prow, "x[3,16]"], 
                                        IC[prow, "x[4,16]"], 
                                        IC[prow, "x[5,16]"]),
                                   beta0=params[prow, "beta0"],
                                   beta1=params[prow, "beta1"],
                                   reg=c(aNew.mc.reg1,
                                         aNew.mc.reg2,
                                         aNew.mc.reg3,
                                         aNew.mc.reg4,
                                         aNew.mc.reg5),
                                   tau_yr=aNew.mc.y,
                                   Q=Qmc, #Process error
                                   n=Nmc,
                                   S=S_u)

N.I.ci = apply(N.I,2,quantile,c(0.025,0.5,0.975))
N.IP.ci = apply(N.IP,2,quantile,c(0.025,0.5,0.975))
N.IPD.ci = apply(N.IPD,2,quantile,c(0.025,0.5,0.975))
N.IPDE.ci = apply(N.IPDE,2,quantile,c(0.025,0.5,0.975))
N.IPDEA.ci = apply(N.IPDEA,2,quantile,c(0.025,0.5,0.975))


uncertainty_out<-list(N.det, N.I.ci, N.IP.ci, N.IPD.ci, N.IPDE.ci, N.IPDEA.ci) #or as.df??

return(uncertainty_out)
}