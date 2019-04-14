
#Manually transcribing data from Hughes dataset, Florida Keys location.
#Coordinates provided were 24.8 N, 80.9 W. The site size was 750 km2

HughesData <- function() {
  
Hughes <- data.frame( "LATITUDE" = NA, "LONGITUDE" = NA, 
                     "YEAR" = 1980:2016,
                     "SEVERITY_CODE" = c(1, 0, 0, 2, 0, 1, 0, 2, 0, 1, 2, 0, 0, 0, 0, 0, 0,
                                     2, 2,1,1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1,  0, 0, 1, 2, 0))

return(Hughes)
}