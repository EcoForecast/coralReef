##Script to create the forecast jags model
##'
##' @param data 
##' @param nchain
##' @import rjags
##' @export
createCoralForecastModelContinuous <- function(data,nchain){
  # inits <- list()
  # for(i in 1:5){
  #   inits[[i]] <- list(beta0 = rnorm(1,0,0.2), beta1 = rnorm(1,4,0.1))
  # }
  # print(inits)
  coralForecastModel <- "
  model{
  ##Loop over regions
  for(r in 1:nr){
  #x[r,1] <- 0
  for(t in 2:nt){
    rec[r, t-1] ~ dgamma(6, 10) #Informed prior on bleaching recovery time
    layover[r,t] <- x[r,t-1] - rec[r, t-1]
    mu[r,t] <- beta0 * layover[r,t] + beta1 * S[r,t] + year[t] + reg[r] ##Process model
    xl[r,t] ~ dnorm(mu[r,t],tau_proc) 
    x[r,t]<- min(xl[r,t], 1) #cap at 100% bleaching
  }
  reg[r] ~ dnorm(0,tau_reg) ## individual effects
  }  ## end loop over regions
  for(t in 1:nt){ 
    year[t] ~ dnorm(0,tau_yr)   ## year effects
  }
  #### Priors
    beta0 ~ dnorm(0.0341,10000) ##slope ##Based off of Eakin et al.(2010)
    beta1 ~ dnorm(0.2694,4) ##Intercept
    tau_reg ~ dgamma(1,0.1)
    tau_yr  ~ dgamma(1,0.1)
    tau_proc ~ dgamma(1,0.1)
    for (r in 1:nr){
      layover[r,1] ~ dgamma(6,10)
    }
  }
  "

  j.model   <- jags.model(file = textConnection(coralForecastModel),
                          data = data,
                          n.chains=nchain) # inits = inits,
}

