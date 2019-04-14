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
    mu[r,t] <- beta0 * x[r,(t-1)]+ beta1 * S[r,t] + year[t] + reg[r] ##Process model
    x[r,t] ~ dnorm(mu[r,t],tau_proc)
  }
  reg[r] ~ dnorm(0,tau_reg) ## individual effects
  }  ## end loop over regions
  for(t in 1:nt){ 
    year[t] ~ dnorm(0,tau_yr)   ## year effects
  }
  #### Priors
    beta0 ~ dnorm(3.41,1) ##slope ##Based off of Eakin et al.(2010)
    beta1 ~ dnorm(26.94,0.04) ##Intercept
    tau_reg ~ dgamma(1,0.1)
    tau_yr  ~ dgamma(1,0.1)
    tau_proc ~ dgamma(1,0.1)
  }
  "

  j.model   <- jags.model(file = textConnection(coralForecastModel),
                          data = data,
                          n.chains=nchain) # inits = inits,
}

