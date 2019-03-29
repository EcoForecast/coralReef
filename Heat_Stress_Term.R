
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

library(tidyr)
library(dplyr)
library(reshape2)
library(zoo)
data<-read.csv(file = "Dry_Tortugas.csv")

data1.5<-separate(data, dateVals, c("year","month","day.hr"), sep = "-", remove = TRUE, convert = FALSE)
data2<-separate(data1.5, day.hr, c("day", "hr"), sep = " ", remove = TRUE, convert = FALSE)
Monthly_means <- data2 %>%
  group_by(month) %>%
  summarise(SST_climatology = mean(SSTvals,na.rm=TRUE))

MMM_SST_climatology<- max(Monthly_means$SST_climatology)#hottest mean month
sub.data <- data2 %>%
  group_by(year,month,day) %>%
  summarise(dailySST = mean(SSTvals,na.rm=TRUE))

biweekly.data <- sub.data[sub.data$day %in% c('02','05','09',12,15,18,22,27), ]

for(i in 1:length(biweekly.data$dailySST)){
  biweekly.data$hotspot[i] <- biweekly.data$dailySST[i] - MMM_SST_climatology
}


biweekly.data$sum <- rollsumr(biweekly.data$hotspot, k = 24, fill = NA)
biweekly.data$DHWs <- biweekly.data$sum * 0.5

biweekly.data$stress <- ifelse(biweekly.data$hotspot > 0 && biweekly.data$hotspot <1 , 'Bleaching Watch', NA)
biweekly.data$stress <- ifelse(biweekly.data$DHWs < 0, 'No stress')
biweekly.data$stress <- ifelse(biweekly.data$hotspot > 0 && biweekly.data$hotspot <1 , 'Bleaching Watch', NA)
biweekly.data$stress <- ifelse(biweekly.data$DHWs > 4, 'Bleaching Alert lvl 1')

biweekly.data$stress<-ifelse(biweekly.data$hotspot < 0, 'No stress', 
       ifelse(biweekly.data$hotspot > 0 , 'Bleaching Watch', 
              ifelse(biweekly.data$DHWs > 4 && biweekly.data$hotspot < 8, 'Bleaching Alert lvl 1',
                    ifelse(biweekly.data$DHWs > 8 , 'Bleaching Alert lvl 2', NA)
              )      
       )
)
