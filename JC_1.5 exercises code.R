###############################
## Joel Cabrera
## Chapter 1 Exercises
##############################

## Load data
#UNpop.dta
#UNpop.txt
library(foreign)
#undata <- read.dta("UNpop.dta")
#undata <- read.table("UNpop.txt")
data <- read.csv('turnout.csv')
data <-read.csv('turnout.csv', head=T, sep=",")

## Dimensions
dim(data)

## Summarize
summary(data)

## Names
names(data)

## Type of object
class(data)
class(data[, "VAP"])


## Calculating VAP turnout rate
# data(total/VAP) does not work
#data[, "total"]
#data[, "VAP"]
#data[, "osvoters"]
#data[, "total"]/data[, "VAP"]
#data$total/data$VAP
#VAPvt <- data[, "total"]/data[, "VAP"]
data$VAPvt <- data$total/data$VAP
data$VAPvt

## Calculate VEP turnout out
data$VEPvt <- data$total/data$VEP
data$VEPvt

## Calculate differences between VAP and ANES
#data[, "ANES"]
#ANESvt <- data[, "ANES"]
#(VAPvt*100)[1] -
#ANESvt[1]
#VAPvt*100 - ANESvt
data$VAPvt100 <- data$VAPvt*100 
data$VAPANES <- data$VAPvt100 - data$ANES
mean(data$VAPANES)
range(data$VAPANES)

## Calculate difference between VEPvt and ANES vt
data$VEPvt100 <- data$VEPvt*100
data$VEPANES <- data$VEPvt100 - data$ANES
mean(data$VEPANES)

## Presidential years
data$year[c(1, 3, 5, 7, 9, 11, 13, 14)]
data$VEPvt[c(1, 3, 5, 7, 9, 11, 13, 14)]
data$ANES[c(1, 3, 5, 7, 9, 11, 13, 14)]

data[, "VAP"]
data[c(1, 2, 3, 4, 5, 6, 7),]
data[c(1, 2, 3, 4, 5, 6, 7),"VAP"]
data$VAP[c(1, 2, 3, 4, 5, 6, 7)]


turnout1 <- data[c(1, 2, 3, 4, 5, 6, 7),]
data[c(8, 9, 10, 11, 12, 13, 14),]
turnout2 <- data[c(8, 9, 10, 11, 12, 13, 14),]


## Problem 6
(data[, "felons"])
(data[, "noncit"])
(data[, "felons"]) + (data[, "noncit"])
fnon <- (data[, "felons"]) + (data[, "noncit"])
data[, "VAP"] - fnon
adjVAP <- (data[, "VAP"] - fnon)
data$adjVAP <- adjVAP

data[, "overseas"]
data[, "total"]
to <- data[, "total"] - data[, "overseas"]
adjVAPvt <- to/adjVAP
(adjVAPvt*100)[14]

turnout1[, "VEP"]
turnout1[, "total"]
turnout1[, "ANES"]
turnout1[, "total"]/turnout1[, "VEP"]
turnout1VEPvt <- (turnout1[, "total"]/turnout1[, "VEP"])-(turnout1[, "ANES"])
turnout1VEPvt
turnout2[, "VEP"]
turnout2[, "total"]
turnout2[, "ANES"]
turnout2[, "total"]/turnout2[, "VEP"]
turnout2VEPvt <- (turnout2[, "total"]/turnout2[, "VEP"])-(turnout2[, "ANES"])
turnout2VEPvt
