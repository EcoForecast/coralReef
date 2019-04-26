
#function based on https://coralreefwatch.noaa.gov/satellite/methodology/methodology.php

#calculate MMM_SST_climatology by taking mean for hottest month 

# HotSpot = SST - MMM_SST_climatology #do this twice-weekly, MMM_SST_climatology stays constant 
# 
# DHWs = 0.5 * Summation of previous 24 twice-weekly HotSpots
# 
# Stress Level	   	Definition	   			                  Effect
# No Stress		          HotSpot <= 0				               ---
#   Bleaching Watch	0 < HotSpot < 1			                   ---
#   Bleaching Warning	1 <= HotSpot and 0 < DHW < 4		    Possible Bleaching
# Bleaching Alert Level 1	1 <= HotSpot and 4 <= DHW < 8		Bleaching Likely
# Bleaching Alert Level 2	   1 <= HotSpot and 8 <= DHW		Mortality Likely 

##Function to calculate hot spot values for the 5 regions
##'
##' @param years The desired years
##' @import tidyr
##' @import dplyr
##' @import reshape2
##' @import zoo
##' @export
Heat_Stress_Term <- function(years) {
  #BB = BiscayneBay, MK = MiddleKeys, LK = LowerKeys, DT = DryTortugas, UK = UpperKeys
  #Output Format = matrix with three columns (region, dateVals, SSTVals)
  BB <- read.csv("Biscayne_Bay.csv", colClasses=c(NA, "NULL", NA, NA))
  bb <- rep("BB", nrow(BB))
  BB$X.1 <- bb
  
  MK <- read.csv("Middle_Keys.csv", colClasses=c(NA, "NULL", NA, NA))
  mk <- rep("MK", nrow(MK))
  MK$X.1 <- mk
  
  LK <- read.csv("Lower_Keys.csv", colClasses=c(NA, "NULL", NA, NA))
  lk <- rep("LK", nrow(LK))
  LK$X.1 <- lk
  
  DT <- read.csv("Dry_Tortugas.csv", colClasses=c(NA, "NULL", NA, NA))
  dt <- rep("DT", nrow(DT))
  DT$X.1 <- dt
  
  UK <- read.csv("Upper_Keys.csv", colClasses=c(NA, "NULL", NA, NA))
  uk <- rep("UK", nrow(UK))
  UK$X.1 <- uk
  
  keys1 <- rbind(BB, MK)
  keys2 <- rbind(keys1, LK)
  keys3 <- rbind(keys2, DT)
  keys <- rbind(keys3, UK)
  
  #Cleaning up
  library(tidyr)
  colnames(keys)[1] <- "region"
  keys <- separate(keys, dateVals, c("year","month","day.hr"), sep = "-", remove = TRUE, convert = FALSE)
  keys <-separate(keys, day.hr, c("day", "hr"), sep = " ", remove = TRUE, convert = FALSE)
  
  Monthly_means <- keys %>%
    group_by(region, month) %>%
    summarise(SST_climatology = mean(SSTvals,na.rm=TRUE))
  
  MMM_SST_climatology<- max(Monthly_means$SST_climatology)#hottest mean month
  sub.data <- keys %>%
    group_by(region,year,month,day) %>%
    summarise(dailySST = mean(SSTvals,na.rm=TRUE))
  
  biweekly.data <- sub.data[sub.data$day %in% c('02','05','09',12,15,18,22,27), ]
  
  for(i in 1:length(biweekly.data$dailySST)){
    biweekly.data$hotspot[i] <- biweekly.data$dailySST[i] - MMM_SST_climatology
  }
  
  
  biweekly.data$sum <- rollsumr(biweekly.data$hotspot, k = 24, fill = NA)
  biweekly.data$DHWs <- biweekly.data$sum * 0.5
  
  biweekly.data <-  na.omit(biweekly.data)
  count_df<- biweekly.data %>% group_by(year, region) %>%
    summarise(count.hotspot = sum(hotspot>0))
  
  final_df <- as.data.frame(t(count_df))
  
  ##Format to be the desired dimensions (rows being regions, columns being years)
  output <- matrix(nrow=5,ncol=length(years))
  regions <- c("LK","MK","UK","BB","DT")
  for(y in 1:length(years)){
    for(r in 1:length(regions)){
      subDat <- final_df[,(final_df[2,]==regions[r] &final_df[1,]==years[y])]
      if(length(subDat)==3){
        output[r,y] <- as.numeric(as.character(subDat[3]))
      }
    }
  }
  #output[is.na(output)] <- 0
  
  # biweekly.data$stress <- ifelse(biweekly.data$hotspot > 0 && biweekly.data$hotspot <1 , 'Bleaching Watch', NA)
  # biweekly.data$stress <- ifelse(biweekly.data$DHWs < 0, 'No stress')
  # biweekly.data$stress <- ifelse(biweekly.data$hotspot > 0 && biweekly.data$hotspot <1 , 'Bleaching Watch', NA)
  # biweekly.data$stress <- ifelse(biweekly.data$DHWs > 4, 'Bleaching Alert lvl 1')
  return(output)
}