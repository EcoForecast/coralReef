##' Creates the calibration ensemble
##' @param out.mat
##' @param Nmc The number of ensembles
##' @export
createCalEnsemble <- function(out.mat,Nmc,nYears){
  rndNums <- sample(1:nrow(out.mat),Nmc,replace=F)
  beta0 <- out.mat$beta0[rndNums]
  beta1 <- out.mat$beta1[rndNums]
  reg <- rbind(out.mat$'reg[1]'[rndNums],
               out.mat$'reg[2]'[rndNums],
               out.mat$'reg[3]'[rndNums],
               out.mat$'reg[4]'[rndNums],
               out.mat$'reg[5]'[rndNums])
  yr <- array(dim=c(5,1,Nmc))
  tau_yr = mean(out.mat$tau_yr)
  
  
  ##Ensemble Kalman Filter
  ##Create ensemble members
  Nmc <- 10
  ##IC are from the model
  ##Need to switch these to be the same samples from each

  ICs <- rbind(out.mat2[rndNums,"x[1,16]"],
               out.mat2[rndNums,"x[2,16]"],
               out.mat2[rndNums,"x[3,16]"],
               out.mat2[rndNums,"x[4,16]"],
               out.mat2[rndNums,"x[5,16]"])
  beta0 <- out.mat$beta0[rndNums]
  beta1 <- out.mat$beta1[rndNums]
  reg <- rbind(out.mat$'reg[1]'[rndNums],
               out.mat$'reg[2]'[rndNums],
               out.mat$'reg[3]'[rndNums],
               out.mat$'reg[4]'[rndNums],
               out.mat$'reg[5]'[rndNums])
  yr <- array(dim=c(5,1,Nmc))
  tau_yr = mean(out.mat$tau_yr)
  #year needs to be the same when tau_yr is the same (for all years per run)
  for(r in 1:5) {
    if((max(0, tau_yr)==0) | tau_yr==0) {
      yr[r,1,] <- rnorm(1, 0, 0)
    }
    else{
      yr[r,1,] <- rnorm(1, 0, 1/sqrt(tau_yr))
    }
  }
  rec <- array(dim=c(5,1,Nmc))
  for(r in 1:5){
    rec[r,1,] <- rgamma(Nmc,6,10) #Distribution same as jags model
  }
  Q <- out.mat$tau_proc[rndNums]

  ##Q is process error from the model
  ##n is the size of the monte carlo ensemble
  ##S is the forecasted covariates
  
  output <- list(beta0=beta0,beta1=beta1,reg=reg,year=yr,rec=rec,Q=Q,n=Nmc,nt=1,ICs=ICs)
  return(output)
}

  
  
  