##' Kalman Filter: Analysis step
##' @param  mu.f = Forecast mean (vector)
##' @param  P.f  = Forecast covariance (matrix)
##' @param  Y    = observations, with missing values as NAs) (vector)
##' @param  R    = observation error covariance (matrix)
##' @param  H    = observation matrix (maps observations to states)
KalmanAnalysis <- function(mu.f,P.f,Y,R,H){
  ##Loop over regions
  nr <- 5
  for(r in 1:nr){
      x[r] ~ dnorm(mu.f[r],p.f) 
  }
  obs = !is.na(Y) ## which Y's were observed?
  if(any(obs)){
    H <- H[obs,]                                              ## observation matrix
    K <- P.f %*% t(H) %*% solve(H%*%P.f%*%t(H) + R[obs,obs])  ## Kalman gain (the same)
    mu.a <- mu.f + K%*%(Y[obs] - H %*% mu.f)                  ## update mean
    P.a <- (1-K %*% H)*P.f                                    ## update covariance
  } else {
    ##if there's no data, the posterior is the prior
    mu.a = mu.f
    P.a = P.f
  }
  return(list(mu.a=mu.a,P.a=P.a))
}
