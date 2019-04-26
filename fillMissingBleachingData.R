##Function to compute the bleaching data for each region
##' 
##' @param dat Data object in the form of a list with two elements: data$b and data$S
##' @export
fillMissingBleachingData <- function(dat) {
  subVals <- dat$S[is.na(dat$x)]*.0341 + .2694
  for(i in 1:length(subVals)){
    subVals[i] <- rnorm(1,subVals[i],.1)
  }
  dat$x[is.na(dat$x)] <- subVals
  dat$x[dat$x>1] <- 1

  return(dat)
}