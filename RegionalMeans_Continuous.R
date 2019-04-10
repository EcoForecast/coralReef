#Continuous scale for donner data bleaching


##Function to consolidate regional datasets and add Hughes data
##'
##' @param df Dataframe with region data
##' @param years The desired years
##' @export

RegionalMeans_continuous=function(df,years,Hughes) {
  Hughes <- HughesData_continuous()
  x=c()
  sev=c()
  y=unique(df$YEAR)
  for (a in 1:length(y)) {
    b=df$SEVERITY_CODE[(df$YEAR==y[a])]
    x=c(x, b)
    sev=c(sev, mean(x))
  }
  #Donner data --> -1 unknown, 0 no bleaching, 1 1-10%, 2 10-50%, 3 >50%
  for (a in 1:length(sev)) {
    if (sev[a]==1) {sev[a]=runif(1, min=1, max=10)}
    if (sev[a]==2) {sev[a]=runif(1, min=10, max=50)}
    if (sev[a]==3) {sev[a]=runif(1, min=50, max=100)}
    
  }
  
  df1=data.frame(LATITUDE=mean(df$LATITUDE), LONGITUDE=mean(df$LONGITUDE), YEAR=unique(df$YEAR), SEVERITY_CODE=round(sev))
  #Removing duplicated years from Hughes
  hix=c()
  for (a in 1:length(y)) {
    ix=Hughes$YEAR==y[a]
    hix=c(hix, which(ix==TRUE))
  }
  Hughes1=Hughes[-c(hix),]
  #Binding reduced Hughes dataset with aggregated regional data
  df2 = rbind(df1, Hughes1)
  df2 <- df2[order(df2$YEAR),]
  
  return(df2)
}

