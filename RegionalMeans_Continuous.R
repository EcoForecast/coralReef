#Continuous scale for donner data bleaching


##Function to consolidate regional datasets and add Hughes data
##'
##' @param df Dataframe with region data
##' @param years The desired years
##' @export

RegionalMeans_continuous=function(df,years,Hughes) {
  Hughes <- HughesData_continuous()

  #Donner data --> -1 unknown, 0 no bleaching, 1 1-10%, 2 10-50%, 3 >50%

  while(min(df[,5])<0){ #when row is removed, that index gets skipped - this keeps the loop going
  for (a in 1:nrow(df)) {
    if(is.na(df[a,5])) {break} #so it doesn't throw boolean NA
    if(df[a,5]<0) {df=df[-c(a),]}
  }}
  
  for (a in 1:nrow(df)){
    if (df[a,5]==1) {df[a,5]=runif(1, min=1, max=10)}
    if (df[a,5]==2) {df[a,5]=runif(1, min=10, max=50)}
    if (df[a,5]==3) {df[a,5]=runif(1, min=50, max=100)}
    df[a,5]=df[a,5]/100
    if(df[a,5]<0) {df=df[-c(a),]}
  }
  
  #Making means for all years (years are already continuous values)
  x=c()
  sev=c()
  y=unique(df$YEAR)
  for (a in 1:length(y)) {
    b=df$SEVERITY_CODE[(df$YEAR==y[a])]
    x=c(x, b)
    sev=c(sev, mean(x))
  }
  
  df1=data.frame(LATITUDE=mean(df$LATITUDE), LONGITUDE=mean(df$LONGITUDE), YEAR=unique(df$YEAR), SEVERITY_CODE=sev)
  #Removing duplicated years from Hughes
  #hix=c()
  #for (a in 1:length(y)) {
  #  ix=Hughes$YEAR==y[a]
  #  hix=c(hix, which(ix==TRUE))
  #}
  #Hughes1=Hughes[-c(hix),]
  Hughes1=NA
  #Binding reduced Hughes dataset with aggregated regional data
  df2 = rbind(df1, Hughes1)
  df2 <- df2[order(df2$YEAR),]
  
  #df1 <- df1[order(df1$YEAR),]
  
  
  return(df2)
}

