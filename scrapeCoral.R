#pull data from simondonner database
library(readxl)
library(httr)
url1<-"http://simondonner.com/wp-content/uploads/2017/01/Bleaching-database-V1.0.xlsx"
GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
require(XLConnect)
wb = loadWorkbook(tf)
df = readWorksheet(wb, sheet = "Bleaching database V1.0", header = TRUE)



#pull data from hughes paper
library(tabulizer)
library(miniUI)
#drag over desired table area
areas<-extract_areas("http://science.sciencemag.org/content/sci/suppl/2018/01/03/359.6371.80.DC1/aan8048_Hughes_SM.pdf", guess=FALSE, pages=19)
df<-data.frame(areas[[1]])
FK<-df[9,]
dates<-c(1980:2016)

#Can also extract with using shiny tools, but gives worse table. 
gti_table <- extract_tables(
  "http://science.sciencemag.org/content/sci/suppl/2018/01/03/359.6371.80.DC1/aan8048_Hughes_SM.pdf",
  output = "data.frame",
  pages = c(19), 
  guess = FALSE
)
