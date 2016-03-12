########################
# Maritime Piracy - Analysis
#######################
library(aod)
library(MASS)
library(AER)

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd(""),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

piracy <- read.csv("piracy.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = c("", "NA"))
piracy$X <- NULL
piracy$bcratio <- (piracy$borderkm/piracy$coastkm)
piracy$p2sq <- ((piracy$polity2)^2)
piracy$cmsq <- ((piracy$cmort)^2)

# Dynamically Link to first R script file
#source("MergeAll.R")

summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))

summary(P1 <- glm(incidents ~ cmort + pop.gr + battlebest
                   + ND
                   + Atcat + unem.total
                   + bcratio + p2sq,
                   family="poisson",
                   data = piracy))

summary(P2 <- glm(incidents ~ cmort + pop.gr + battlebest
                  + ND
                  + Atcat + unem.y
                  + bcratio + p2sq,
                  family="poisson",
                  data = piracy))

#Negative Binomial model
summary(P3 <- glm(incidents ~ cmsq + pop.gr + battlebest
                     + ND
                     + Atcat + unem.y
                     + bcratio + p2sq,
                     family="poisson",
                     data = piracy))

#Negative Binomial model
summary(P4 <- glm(incidents ~ cmsq + pop.gr + battlebest
                  + ND
                  + mobilep100 + unem.y
                  + bcratio + p2sq,
                  family="poisson",
                  data = piracy))

dispersiontest(P1, trafo=0)
dispersiontest(P2, trafo=0)
dispersiontest(P3, trafo=0)
dispersiontest(P4, trafo=0)
