#pull data from simondonner database
library(readxl)
library(httr)
url1<-"http://simondonner.com/wp-content/uploads/2017/01/Bleaching-database-V1.0.xlsx"
GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
require(XLConnect)
wb = loadWorkbook(tf)
df = data.frame(readWorksheet(wb, sheet = "Bleaching database V1.0", header = TRUE))
library(dplyr)
df<-filter(df, COUNTRY=="Florida (USA)")