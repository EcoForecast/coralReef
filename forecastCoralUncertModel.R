##' Forecast model for uncertainty analysis
##'
##' @param IC Initial Conditions
##' @param beta0 Slope
##' @param beta1 Intercept
##' @param reg Region random effects
##' @param year Year random effects
##' @param rec Bleaching recovery rate
##' @param Q Process error (default = 0 for deterministic runs)
##' @param n Size of Monte Carlo ensemble
##' @param S Heat stress covariate
##' @param nr The number of regions
##' @param nt The number of years
##' @export
ForecastCoralUncertModel <- function(IC, beta0, beta1, reg, year, rec, Q, n, S,nr=5,nt=13){
  x <- array(dim=c(nr,nt,n))
<<<<<<< HEAD
=======
  Xprev <- IC
>>>>>>> b935f7cbd8a847bff4003d7150c30030b5a0bde0
    for(r in 1:nr){
      Xprev <- IC[r,1] #region 1, time 1
      for(i in 1:n){
        for (t in 1:t) {
          #layover=rec[r,t,i] * Xprev
          mu <- beta0[i] * Xprev - rec[r,t,i] + beta1[i] * S[r,t,i]+ year[r,t,i] + reg[r,i] ##Process model
          x[r,t,i] <- rnorm(1,mu,Q)
          x[r,t,i] = max(x[r,t,i], 0)
          x[r,t,i] = min(x[r,t,i], 1) #0-100%
          Xprev <- x[r,t,i]
          if(t==nt & i<n) {
            Xprev <- IC[r,i+1] #region through IC i
          }
          if(t==nt & i==n & r<5){
            Xprev <- IC[r+1,1] #region 2-5, IC i when done with iterations
          }
        }
      }
    }
  #x starts in 2004, goes to 2016 - rows. Also 5 regions, number of iterations (sample size)
  return(x)
}
