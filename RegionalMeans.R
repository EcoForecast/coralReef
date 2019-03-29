##Function to consolidate regional datasets and add Hughes data
##'
##' @param df Dataframe with region data
##' @param years The desired years
##' @export
RegionalMeans=function(df,years) {
  x=c()
  sev=c()
  y=unique(df$YEAR)
  for (a in 1:length(y)) {
    b=df$SEVERITY_CODE[(df$YEAR==y[a])]
    x=c(x, b)
    sev=c(sev, mean(x))
  }
  
  df1=data.frame(LATITUDE=mean(df$LATITUDE), LONGITUDE=mean(df$LONGITUDE), YEAR=unique(df$YEAR), SEVERITY_CODE=round(sev,digits=0))
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