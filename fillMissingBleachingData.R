##Function to compute the bleaching data for each region
##' 
##' @param dat Data object in the form of a list with two elements: data$b and data$S
##' @export
fillMissingBleachingData <- function(dat) {
  if(ncol(dat$y)>1){
  for(i in 1:nrow(dat$y)){
    dat$y[i,1] <- 0
    for(j in 2:ncol(dat$y)){
      rec <- rgamma(1,6,10)
      layover <- rec*dat$y[i,(j-1)]
      if(layover>1){layover=1}
      if(is.na(dat$y[i,j])){
        subVal <- dat$S[i,j]*.0341 + .2694+layover
        dat$y[i,j] <- rnorm(1,subVal,0.1)
      }
    }
  }
}
else{
  for(i in 1:length(dat$y)){
    rec <- rgamma(1,6,10)
    subVal <- dat$S[i]*.0341 + .2694+rec
    dat$y[i] <- rnorm(1,subVal,0.1)
  }
 
}
  dat$y[dat$y>1] <- 1

  return(dat)
}
