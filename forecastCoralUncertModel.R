##' Forecast model for uncertainty analysis
##'
##' @param IC Initial Conditions
##' @param beta0 Slope
##' @param beta1 Intercept
##' @param reg Region random effects
##' @param tau_yr Year random effects
##' @param Q Process error (default = 0 for deterministic runs)
##' @param n Size of Monte Carlo ensemble
##' @param S Heat stress covariate
##' @export
ForecastCoralUncertModel <- function(IC,beta0,beta1,reg, year, Q,n,S){
  nr=5
  nt=13
  x <- array(dim=c(nr,nt,n))
  for (i in 1:n) {
    Xprev <- IC[r,i]
    for(r in 1:nr){
      for(t in 1:nt){
        rec <- rgamma(1,1, 10)
        layover=rec * Xprev
        mu <- beta0[n] * layover + beta1[n] * S[r,t,n]+ year[r,t,n] + reg[r,n] ##Process model
        x[r,t,i] <- rnorm(1,mu,Q)
        x[r,t,i] = max(x[r,t,i], 0)
        x[r,t,i] = min(x[r,t,i], 1) #0-100%
        Xprev <- x[r,t,i]
      }
    }
  }
  #x starts in 2004, goes to 2016 - rows. Columns are 5 regions
  return(x)
}