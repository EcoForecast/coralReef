#NOAA Coral Reef Watch Time Series Bleaching 2000-2019

library(tidyr)

DT<- read.table(file="https://www.ospo.noaa.gov/data/cb/TS_vs/vs_ts_DryTortugas_Florida.txt", header=F, sep="",stringsAsFactors = F, fill=TRUE)
DT<- unite(DT, "Reef_Name", c("V15","V16"))
DT$Reef_Name <- gsub( ",", "", as.character(DT$Reef_Name))
DT <- DT[,1:15]
colnames(DT)<-DT[1,]
DT<-DT[-1,]

BB <- read.table(file="https://www.ospo.noaa.gov/data/cb/TS_vs/vs_ts_Biscayne_Florida.txt", header=FALSE, sep="", fill=TRUE, stringsAsFactors = FALSE)
BB$V15 <- gsub( "Biscayne,", "Biscayne_Bay", as.character(BB$V15))
BB <- BB[,1:15] #exclude last row since not necessary
colnames(BB)<-BB[1,] #set column names as first row
BB <- BB[-1,] #get rid of first row since it is a replicate of the column names
colnames(BB)[colnames(BB)=="Reef_Name"] <- "Reef_Name_"

LK <- read.table(file="https://www.ospo.noaa.gov/data/cb/TS_vs/vs_ts_LowerKeys_Florida.txt", header=FALSE, sep="", fill=TRUE, stringsAsFactors = FALSE)
LK <- unite(LK, "Reef_Name", c("V15","V16"))
LK$Reef_Name <- gsub( ",", "", as.character(LK$Reef_Name))
LK <- LK[,1:15]
colnames(LK)<-LK[1,]
LK <- LK[-1,]

MK <- read.table(file="https://www.ospo.noaa.gov/data/cb/TS_vs/vs_ts_MiddleKeys_Florida.txt", header=FALSE, sep="", fill=TRUE, stringsAsFactors = FALSE)
MK <- unite(MK, "Reef_Name", c("V15","V16"))
MK$Reef_Name <- gsub( ",", "", as.character(MK$Reef_Name))
MK <- MK[,1:15]
colnames(MK)<-MK[1,]
MK <- MK[-1,]

UK <- read.table(file="https://www.ospo.noaa.gov/data/cb/TS_vs/vs_ts_UpperKeys_Florida.txt", header=FALSE, sep="", fill=TRUE, stringsAsFactors = FALSE)
UK <- unite(UK, "Reef_Name", c("V15","V16"))
UK$Reef_Name <- gsub( ",", "", as.character(UK$Reef_Name))
UK <- UK[,1:15]
colnames(UK)<-UK[1,]
UK <- UK[-1,]

Bleaching_Watch <- rbind(BB, MK, LK, DT, UK)

Bleaching_Watch$percbleach <- (as.numeric(Bleaching_Watch$DHW)*3.41 + 26.94)
#Equation taken from Eakin et al. (2010)

