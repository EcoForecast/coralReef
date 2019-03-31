##Converting the SST data into a single data frame

#BB = BiscayneBay, MK = MiddleKeys, LK = LowerKeys, DT = DryTortugas, UK = UpperKeys
#Output Format = matrix with three columns (region, dateVals, SSTVals)
BB <- read.csv("BB.csv")
bb <- rep("BB", nrow(BB))
BB$X <- bb

MK <- read.csv("MK.csv")
mk <- rep("MK", nrow(MK))
MK$X <- mk

LK <- read.csv("Dry_Tortugas.csv")
lk <- rep("LK", nrow(LK))
LK$X <- lk

DT <- read.csv("DT.csv")
dt <- rep("DT", nrow(DT))
DT$X <- dt

Uk <- read.csv("UK.csv")
uk <- rep("UK", nrow(UK))
UK$X <- uk

keys1 <- rbind(BB, MK)
keys2 <- rbind(keys1, Lk)
keys3 <- rbind(keys2, DT)
keys <- rbind(keys3, UK)
colnames(keys1)[1] <- "region"

