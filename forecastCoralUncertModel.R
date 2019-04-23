##' Forecast model for uncertainty analysis
##'
##' @param IC Initial Conditions
##' @param beta0 Slope
##' @param beta1 Intercept
##' @param year Year random effects
##' @param reg Region random effects
##' @param Q Process error (default = 0 for deterministic runs)
##' @param n Size of Monte Carlo ensemble
##' @param S Heat stress covariate
##' @export
forecastCoralUncertModel <- function(IC,beta0,beta1,year,reg,Q=0,n,S){
  x <- array(dim=c(nr,nt,n))
  for(r in 1:nr){
    Xprev <- IC[r]
    print(Xprev)
  for(t in 1:nt){
    mu <- beta0 * Xprev + beta1 * S[r,t]+ year[t] + reg[r] ##Process model
    print(mu)
    x[r,t,] <- rlnorm(n,mu,Q)
    Xprev <- x[r,t,]
    #x[r,t] <-  dnorm(mu[r,t],tau_proc)
  }
  }
  return(x)
}

