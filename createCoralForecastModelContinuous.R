##Script to create the forecast jags model
##'
##' @param data Data object
##' @param nchain Number of chains
##' @import rjags
##' @export
createCoralForecastModelContinuous <- function(data,nchain){
  coralForecastModel <- "
  model{
  ##Loop over regions
  for(r in 1:nr){
    for(t in 2:nt){
      #rec[r, t-1] ~ dgamma(6, 10) #Informed prior on bleaching recovery time
      #layover[r,t] <- x[r,(t-1)] - rec
      mu[r,t] <- beta0 * x[r,(t-1)] - rec + beta1 * S[r,t] + year[t] + reg[r] ##Process model
      xl[r,t] <- min(mu[r,t],1)
      x[r,t] ~ dnorm(xl[r,t],tau_proc) 
    }
    x[r,1] ~ dbeta(2,2)

    for(t in 1:nt){
      y[r,t] ~ dnorm(x[r,t],tau_obs)
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
    tau_obs ~ dgamma(1,0.1)
    for (r in 1:nr){
      layover[r,1] ~ dgamma(6,10)
    }
    rec ~ dgamma(6, 10)
  }
  "

  j.model   <- jags.model(file = textConnection(coralForecastModel),
                          data = data,
                          n.chains=nchain) 
}

