
#Manually transcribing data from Hughes dataset, Florida Keys location.
#Coordinates provided were 24.8 N, 80.9 W. The site size was 750 km2

HughesData <- function() {
  
Hughes <- data.frame("LOCATION" = "Florida Keys", "LATITUDE" = NA, "LONGITUDE" = NA, "MONTH" = NA, "DEPTH"=NA, 
                     "YEAR" = 1980:2016,
                     "SEVERITY_CODE" = c(NA, 0, 0, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, 0, 0,
                                     NA, NA, NA, NA, 0, 0, NA, NA, NA, NA, NA, 0, 0, 0, NA,  0, 0, NA, NA, 0),
                     "PERCENT_BLEACHED" = NA, "MORTALITY_CODE" = NA)

return(Hughes)
}