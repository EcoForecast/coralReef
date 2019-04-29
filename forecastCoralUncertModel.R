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
ForecastCoralUncertModel <- function(IC, beta0, beta1, reg, year, rec, Q, n, S){
  nr=5
  nt=13
  x <- array(dim=c(nr,nt,n))
  Xprev <- IC[1,i]
    for(r in 1:nr){
      for(t in 1:nt){
        for (i in 1:n) {
          layover=rec[r,t,i] * Xprev
          mu <- beta0[i] * layover + beta1[i] * S[r,t,i]+ year[r,t,i] + reg[r,i] ##Process model
          x[r,t,i] <- rnorm(1,mu,Q)
          x[r,t,i] = max(x[r,t,i], 0)
          x[r,t,i] = min(x[r,t,i], 1) #0-100%
          Xprev <- x[r,t,i]
          if(i==n & r<5){
            Xprev <- IC[r+1,i]
          }
        }
      }
    }
  #x starts in 2004, goes to 2016 - rows. Columns are 5 regions
  return(x)
}