##' Adds additional iterations until convergence and large enough effective sample size are met
##'
##' @param j.model The JAGS model
##' @param variableNames Variable names for coda.samples
##' @param maxIter The maximum number of iterations to run before declaring that the model will not converge (optional)
##' @param baseNum The initial number of iterations to run before checking for convergence
##' @param iterSize The number of iterations to iteratively increase before checking for convergence again and large enough sample size (optional)
##' @param ID A identification string to paste with the message of non-convergence (optional)
##' @export
##' @import rjags
##' @import runjags
##' @import ecoforecastR
##' @import coda
runForecastIter <- function(j.model,variableNames,maxIter=10**9,baseNum=5000,iterSize =5000,ID=""){
  jags.out   <- coda.samples (model = j.model,
                              variable.names = variableNames,
                              n.iter = baseNum)
  
  ###Check for Model Convergence and keep adding to MCMC chains if it hasn't converged and/or effective sample size is not large enough
  numb <- baseNum #The current number of iterations at this step
  continue <- TRUE #Flag that signals that the coda.samples should be rerun to produce more iterations because the model hasn't converged yet/doesnt have a large enough sample size
  GBR.bad <- TRUE #Flag that signals that it hasn't converged yet
  burnin <- 0
  while(continue & numb<maxIter){
    print(numb) #Just done to keep track of the number of iterations that have been performed
    new.out   <- coda.samples (model = j.model,
                               variable.names = variableNames,
                               n.iter = iterSize)
    numb <- numb + iterSize
    jags.out <- combine.mcmc(mcmc.objects=list(jags.out,new.out),collapse.chains = FALSE)
    continue <- FALSE
    if(GBR.bad){
      out = list(params=NULL,predict=NULL) #Split output into parameters and state variables
      mfit = as.matrix(jags.out,chains=TRUE)
      pred.cols = grep("x[",colnames(mfit),fixed=TRUE)
      chain.col = which(colnames(mfit)=="CHAIN")
      out$params = ecoforecastR::mat2mcmc.list(mfit[,-pred.cols])
      GBR.vals <- gelman.diag(out$params)
      print(GBR.vals$psrf)
      for(i in 1:nrow(GBR.vals$psrf)){ #Checking to see if any of the parameters haven't converged yet
        for(j in 1:ncol(GBR.vals$psrf)){
          if(!is.nan(GBR.vals$psrf[i,j])){
            if(GBR.vals$psrf[i,j]>1.04){
              continue <-  TRUE
            }
          }
        }
      }
    }
    if(!continue){
      if(burnin==0){ #If the while loop has to be rerun because the effective size is too small, you don't need to calculate burnin again
        GBR <- gelman.plot(out$params)
        burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
        if(length(burnin) == 0) burnin = 1
      }
      var.burn <- window(jags.out,start=burnin)
      out.burn = list(params=NULL,predict=NULL)
      mfit = as.matrix(var.burn,chains=TRUE)
      pred.cols = grep("x[",colnames(mfit),fixed=TRUE)
      chain.col = which(colnames(mfit)=="CHAIN")
      out.burn$params = ecoforecastR::mat2mcmc.list(mfit[,-pred.cols])
      effsize <- effectiveSize(out.burn$params)
      print(effsize)
      for(i in 1:length(effsize)){
        if(effsize[i]<5000){
          continue = TRUE
        }
      }
    }
  }
  if(!continue){
    out.burn$predict = ecoforecastR::mat2mcmc.list(mfit[,c(chain.col,pred.cols)])
    return(out.burn)
  }
  else{
    print(paste(ID, "model did not converge, returning FALSE"))
    return(FALSE)
  }
}