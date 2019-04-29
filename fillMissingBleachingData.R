##Function to compute the bleaching data for each region
##' 
##' @param dat Data object in the form of a list with two elements: data$b and data$S
##' @export
fillMissingBleachingData <- function(dat) {
  subVals <- dat$S[is.na(dat$y)]*.0341 + .2694
  for(i in 1:length(subVals)){
    subVals[i] <- rnorm(1,subVals[i],.1)
  }
  dat$y[is.na(dat$y)] <- subVals
  dat$y[dat$y>1] <- 1

  return(dat)
}