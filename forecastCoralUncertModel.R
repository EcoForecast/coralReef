##' Forecast model for uncertainty analysis
##'
##' @param IC Initial Conditions
##' @param beta0 Slope
##' @param beta1 Intercept
##' @param year Year random effects
##' @param reg Region random effects
##' @param rec Recovery
##' @param Q Process error (default = 0 for deterministic runs)
##' @param n Size of Monte Carlo ensemble
##' @param S Heat stress covariate
##' @export
ForecastCoralUncertModel <- function(IC,beta0,beta1,year,reg,rec,Q=0,n,S){
  nr=5
  nt=5
  x <- array(dim=c(nr,nt,n))
  for(r in 1:nr){
    Xprev <- IC[r]
  for(t in 1:nt){
    layover=rec * Xprev
    mu <- beta0 * layover + beta1 * S[r,t]+ year[t] + reg[r] ##Process model
    xl[r,t] <- min(mu[r,t],1)
    x[r,t,] <- rlnorm(n,xl,Q)
    Xprev <- x[r,t,]
    #x[r,t] <-  dnorm(mu[r,t],tau_proc)
  }
  }
  return(x)
}

