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
    x[r,1] ~ dunif(0,1000)
    for(t in 2:nt){
      #mu[r,t] <- beta1 * S[r,t] + beta0 + year[t] + reg[r] ##Process model
      muP[r,t] <- beta0 * b[r,(t-1)]+ beta1 * S[r,t] + year[t] + reg[r]
      x[r,t] ~ dnorm(muP[r,t],tau_proc)
      b[r,t] ~ dcat(probs[r,t,])

      probs[r,t,1] <- ifelse(x[r,t]<lowLimit,0.8,0.1) #dbeta(alpha.p.cloud,beta.p.cloud) #<10%
      probs[r,t,2] <- 1 - probs[r,t,1]- probs[r,t,3]
      probs[r,t,3] <- ifelse(x[r,t]>upperLimit,0.8,0.1)

      #probs[r,t,1] <- 0.6
      #probs[r,t,2] <- 0
      #probs[r,t,3] <- 0.1
      #probs[r,t,1] <- 0.1
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
  tau_proc ~ dgamma(1,0.1)
  #lowLimit <- -10
  upperLimit <- 150
  lowLimit ~ dnorm(0,4) ## Limit to separate no bleaching from medium bleaching
  #upperLimit ~ dnorm(200,0.0004) ##Limit to separate high bleaching from medium bleaching
  }
  "

  j.model   <- jags.model(file = textConnection(coralForecastModel),
                          data = data,
                          n.chains=nchain)
}

