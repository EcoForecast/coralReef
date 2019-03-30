##Script to create the forecast jags model
##'
##' @param data 
##' @param nchain
##' @import rjags
##' @export
createCoralForecastModel <- function(data,nchain){
  coralForecastModel <- "
  model{
  ##Loop over regions
  for(r in 1:nr){
    mu[r,1] ~ dunif(0,1000)
    for(t in 2:nt){
      #mu[r,t] <- beta1 * S[r,t] + beta0 + year[t] + reg[r] ##Process model
      mu[r,t] <- beta0 * mu[r,(t-1)]+ beta1 * S[r,t] + year[t] + reg[r]
      b[r,t] ~ dcat(probs[r,t,])

      probs[r,t,1] <- ifelse(mu[r,t]<lowLimit,0.9,0.1) #dbeta(alpha.p.cloud,beta.p.cloud) #<10%
      probs[r,t,2] <- 1 - probs[r,t,1]- probs[r,t,3]
      probs[r,t,3] <- ifelse(mu[r,t]>upperLimit,0.9,0.1)

      #probs[r,t,1] <- 0.6
      #probs[r,t,2] <- 0
      #probs[r,t,3] <- 0.1
      #probs[r,t,1] <- 0.7
      #probs[r,t,2] <- 0.2
      #probs[r,t,3] <- 0.1
    }
    reg[r] ~ dnorm(0,tau_reg) ## individual effects
  }  ## end loop over regions
  for(t in 1:nt){ 
    year[t] ~ dnorm(0,tau_yr)   ## year effects
  }
  #### Priors
  #beta1 ~ dnorm(mu.b1,p.b1)
  #beta0 ~ dnorm(mu.b0,p.b0)
  beta0 ~ dunif(-100,100)
  beta1 ~ dunif(0,1000)
  tau_reg ~ dgamma(1,0.1)
  tau_yr  ~ dgamma(1,0.1)
  lowLimit ~ dnorm(1000,0.000004) ## Limit to separate no bleaching from medium bleaching
  upperLimit ~ dnorm(90000,0.000004) ##Limit to separate high bleaching from medium bleaching
  }
  "

  j.model   <- jags.model(file = textConnection(coralForecastModel),
                          data = data,
                          n.chains=nchain)
}

