#Manually transcribing data from Hughes dataset, Florida Keys location.
#Coordinates provided were 24.8 N, 80.9 W. The site size was 750 km2
#Bleaching severity: 0 no bleaching, M 1-30%, S >30%
#M and S converted to 1 and 2 in initial transcription
HughesData_continuous <- function() {
  
  Hughes <- data.frame( "LATITUDE" = NA, "LONGITUDE" = NA, 
                        "YEAR" = 1980:2016,
                        "SEVERITY_CODE" = c(1, 0, 0, 2, 0, 1, 0, 2, 0, 1, 2, 0, 0, 0, 0, 0, 0,
                                            2, 2, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1,  0, 0, 1, 2, 0))
  for (a in 1:nrow(Hughes)) {
    if (Hughes[a,4]==1) {Hughes[a,4]=runif(1, min=1, max=30)}
    else if (Hughes[a, 4]==2) {Hughes[a,4]=runif(1, min=30, max=100)}
    #Hughes[a, 4] <- NA
  }
  
  return(Hughes)
}