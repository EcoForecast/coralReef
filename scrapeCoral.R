##' Pull data from simondonner database and subset for Flordai
##'
##' @export
scrapeCoral <- function(){
  #library(readxl)
  #library(httr)

  ##Download Data
  url1<-"http://simondonner.com/wp-content/uploads/2017/01/Bleaching-database-V1.0.xlsx"
  
  #GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
  #require(XLConnect)
  #wb = loadWorkbook(tf)
  #df = data.frame(readWorksheet(wb, sheet = "Bleaching database V1.0", header = TRUE))
  #library(dplyr)
  #df<-filter(df, COUNTRY=="Florida (USA)")
  
  dat <- openxlsx::read.xlsx(url1)
  
  ##Format Data
  FLdat <- dat[dat$COUNTRY=="Florida (USA)",]
  FLdat <- FLdat[!is.na(FLdat$COUNTRY),]
  return(FLdat)
}

