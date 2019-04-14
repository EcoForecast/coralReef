##Function to compute the bleaching data for each region
##' 
##' @param data Data object in the form of a list with two elements: data$b and data$S
##' @export
fillMissingBleachingData <- function(data) {
  subVals <- data$S[is.na(data$x)]*3.41 + 26.94
  for(i in 1:length(subVals)){
    subVals[i] <- rnorm(1,subVals[i],10)
  }
  data$x[is.na(data$x)] <- subVals
  data$x[data$x>100] <- 100

  return(data)
}