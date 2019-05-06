##' Returns confidence intervals with different model uncertanties for uncertainty analysis
##' @param out.mat jags parameter output matrix (beta0, beta1, reg[1:5], tau_reg, tau_proc, tau_yr)
##' @param out.mat2 jags prediction output matrix (x[,16] --> IC)
##' @param S heat stress 1988 to 2016
##' @param Nmc number of ensemble members to run
##' @export 

uncertainty_anal <- function(out.mat, out.mat2, S, Nmc){
  
  params <- as.matrix(out.mat)
  IC <- as.matrix(out.mat2)
  param.mean <-apply(params, 2, mean)
  
  prow <- sample.int(nrow(params), Nmc, replace=TRUE) #random sample of parameter values
  Qmc <- 1/sqrt(params[prow, "tau_proc"]) #Convert to SD
  IC.sample <- matrix(nrow=5, ncol=Nmc)
  IC.sample[1,] = mean(IC[,"x[1,16]"])
  IC.sample[2,] = mean(IC[,"x[2,16]"])
  IC.sample[3,] = mean(IC[,"x[3,16]"])
  IC.sample[4,] = mean(IC[,"x[4,16]"])
  IC.sample[5,] = mean(IC[,"x[5,16]"])
  
  reg.sample <- matrix(nrow=5, ncol=Nmc)
  reg.sample[1,] = param.mean["reg[1]"]
  reg.sample[2,] = param.mean["reg[2]"]
  reg.sample[3,] = param.mean["reg[3]"]
  reg.sample[4,] = param.mean["reg[4]"]
  reg.sample[5,] = param.mean["reg[5]"]

  S.sample <- array(dim=c(5, 13, Nmc))
  for(r in 1:5) {
    for (t in 1:13) {
      S.sample[r,t,] = S[r,t]
    }
  }
  
    yr.sample <- array(dim=c(5,13,Nmc))
    tau_yr = param.mean["tau_yr"]
   #year needs to be the same when tau_yr is the same (for all years per run)
    for(r in 1:5) {
      for (t in 1:13) {
        if((max(0, tau_yr)==0) | tau_yr==0) {
          yr.sample[r,t,] <- rnorm(1, 0, 0)
        }
        else{
          yr.sample[r,t,] <- rnorm(1, 0, 1/sqrt(tau_yr))
        }
      }
    }
    
    rec.sample <- array(dim=c(5, 13, Nmc))
    for (r in 1:5) {
      for (t in 1:13) {
        for (n in 1:Nmc) {
          rec.sample[r,t,]<- rgamma(1,6, 10) #Distribution same as jags model; not worth translating output
        } #recovery varies at region, time, and run. But not between uncertainty analyses
      }
    }
    
  N.det<- ForecastCoralUncertModel(IC=IC.sample, #Mean IC in 2003, after model calibration
                                   beta0=rep(param.mean["beta0"], Nmc),
                                   beta1=rep(param.mean["beta1"], Nmc),
                                   reg=reg.sample,
                                   year=yr.sample,
                                   rec=rec.sample,
                                   Q=0,
                                   n=1,
                                   S=S.sample)
  IC.sample[1,] = IC[prow, "x[1,16]"]
  IC.sample[2,] = IC[prow, "x[2,16]"]
  IC.sample[3,] = IC[prow, "x[3,16]"]
  IC.sample[4,] = IC[prow, "x[4,16]"]
  IC.sample[5,] = IC[prow, "x[5,16]"]
  N.I<- ForecastCoralUncertModel(IC=IC.sample,#Initial condition spread
                                 beta0=rep(param.mean["beta0"], Nmc),
                                 beta1=rep(param.mean["beta1"], Nmc),
                                 reg=reg.sample,
                                 year=yr.sample,
                                 rec=rec.sample,
                                 Q=0,
                                 n=Nmc,
                                 S=S.sample)
  print("1/5")

  reg.sample[1,] = params[prow, "reg[1]"]
  reg.sample[2,] = params[prow, "reg[2]"]
  reg.sample[3,] = params[prow, "reg[3]"]
  reg.sample[4,] = params[prow, "reg[4]"]
  reg.sample[5,] = params[prow, "reg[5]"]

  tau_yr = params[prow, "tau_yr"]
  for (n in 1:Nmc) {
    for(r in 1:5) {
      for (t in 1:13) {
        if((max(0, tau_yr[n])==0) | tau_yr[n]==0) {
          yr.sample[r,t,n] <- rnorm(1, 0, 0)
        }
        else{
          yr.sample[r,t,n] <- rnorm(1, 0, 1/sqrt(tau_yr[n]))
        }
      }
    }
  }

  for (r in 1:5) {
    for (t in 1:13) {
      for (n in 1:Nmc) {
        rec.sample[r,t,n]<- rgamma(1,6, 10)
      } 
    }
  }
  
  N.IP<- ForecastCoralUncertModel(IC=IC.sample,
                                 beta0=params[prow, "beta0"], #Parameters beta0 and beta1
                                 beta1=params[prow, "beta1"],
                                 reg=reg.sample,
                                 year=yr.sample,
                                 rec=rec.sample,
                                 Q=0,
                                 n=Nmc,
                                 S=S.sample)
  print("2/5")
  
  S_u=S.sample
  for (r in 1:nrow(S_u)) {
    for (t in 1:ncol(S_u)) {
      S_u[r,t,] <- rnorm(Nmc, S[r,t], 1) #1 day SD assumed
      for (n in 1:Nmc) {
        S_u[r,t,n] = max(S_u[r,t,n], 0)
      }
    }
  }
  
  N.IPD<- ForecastCoralUncertModel(IC=IC.sample,
                                   beta0=params[prow, "beta0"],
                                   beta1=params[prow, "beta1"],
                                   reg=reg.sample,
                                   year=yr.sample,
                                   rec=rec.sample,
                                   Q=0,
                                   n=Nmc,
                                   S=S_u) #Driver heat stress data
  print("3/5")
  
  N.IPDE<- ForecastCoralUncertModel(IC=IC.sample,
                                    beta0=params[prow, "beta0"],
                                    beta1=params[prow, "beta1"],
                                    reg=reg.sample,
                                    year=yr.sample,
                                    rec=rec.sample,
                                    Q=Qmc, #Process error
                                    n=Nmc,
                                    S=S_u) 
  print("4/5")
  
  tau.mc.reg <- 1/sqrt(params[prow,"tau_reg"])
  aNew.mc.reg <- matrix(nrow=5, ncol=Nmc)
  for(n in 1:Nmc) {
    aNew.mc.reg[1,n] <- rnorm(1,param.mean["reg[1]"],tau.mc.reg[n])
    aNew.mc.reg[2,n] <- rnorm(1,param.mean["reg[2]"],tau.mc.reg[n])
    aNew.mc.reg[3,n] <- rnorm(1,param.mean["reg[3]"],tau.mc.reg[n])
    aNew.mc.reg[4,n] <- rnorm(1,param.mean["reg[4]"],tau.mc.reg[n])
    aNew.mc.reg[5,n] <- rnorm(1,param.mean["reg[5]"],tau.mc.reg[n])
  }
  
  tau.mc.y <- 1/sqrt(params[prow,"tau_yr"])
    for(r in 1:5) {
      for (t in 1:13) {
        yr.sample[r,t,] <- rnorm(Nmc, 0,tau.mc.y[n])
      }
    }
  
  N.IPDEA<- ForecastCoralUncertModel(IC=IC.sample,
                                     beta0=params[prow, "beta0"],
                                     beta1=params[prow, "beta1"],
                                     reg=aNew.mc.reg,
                                     year=yr.sample,
                                     rec=rec.sample,
                                     Q=Qmc, #Process error
                                     n=Nmc,
                                     S=S_u)
  
  print("5/5")
  
  nr=5 #Could use these at top but don't feel like fiddling with code now
  nt=13
  N.I.ci = array(dim=c(nr, nt, 3))
  N.IP.ci = array(dim=c(nr, nt, 3))
  N.IPD.ci = array(dim=c(nr, nt, 3))
  N.IPDE.ci = array(dim=c(nr, nt, 3))
  N.IPDEA.ci = array(dim=c(nr, nt, 3))
  for (r in 1:nr) {
    for(t in 1:nt) {
    N.I.ci[r,t,] = quantile(N.I[r,t,],c(0.025,0.5,0.975))
    N.IP.ci[r,t,] = quantile(N.IP[r,t,],c(0.025,0.5,0.975))
    N.IPD.ci[r,t,] = quantile(N.IPD[r,t,],c(0.025,0.5,0.975))
    N.IPDE.ci[r,t,] = quantile(N.IPDE[r,t,],c(0.025,0.5,0.975))
    N.IPDEA.ci[r,t,] = quantile(N.IPDEA[r,t,],c(0.025,0.5,0.975))
    }
  }
  uncertainty_out<-list(N.det, N.I.ci, N.IP.ci, N.IPD.ci, N.IPDE.ci, N.IPDEA.ci) 
  
  return(uncertainty_out)
}