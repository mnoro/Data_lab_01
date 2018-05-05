#
library(readr)
MIE <- read_csv("data/MIE.csv")
#
# use tables-
# faster
library(data.table)
tMIE <- data.table(MIE)
head(tMIE, 3)
head(MIE)
tables()
tMIE2 <- tMIE
tMIE[, mv:= mean(value)]
tMIE2[,log(value)]
