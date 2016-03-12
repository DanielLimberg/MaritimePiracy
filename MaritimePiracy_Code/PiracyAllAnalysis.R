########################
# Maritime Piracy - Analysis
#######################
library(aod)
library(MASS)
library(pscl)

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

#Linear Probability Model
summary(LPM1 <- lm(incbinary ~ cmort + pop.gr + battlebest
                   + ND
                   + Atcat + unem.total
                   + bcratio + p2sq,
                   data = piracy))

#Logit model
summary(L1 <- glm(incbinary ~ cmort + pop.gr + battlebest
                  + ND
                  + Atcat + unem.total
                  + bcratio + p2sq,
                  data = piracy, family = "binomial"))

#Negative Binomial model
summary(NB1 <- glm.nb(incidents ~ cmort + pop.gr + battlebest
                      + ND
                      + Atcat + unem.total
                      + bcratio + p2sq,
                      data = piracy))

#Linear Probability Model
summary(LPM2 <- lm(incbinary ~ cmort + pop.gr + battlebest
                   + ND
                   + Atcat + unem.y
                   + bcratio + p2sq,
                   data = piracy))

#Logit model
summary(L2 <- glm(incbinary ~ cmort + pop.gr + battlebest
                  + ND
                  + Atcat + unem.y
                  + bcratio + p2sq,
                  data = piracy, family = "binomial"))

#Negative Binomial model
summary(NB2 <- glm.nb(incidents ~ cmort + pop.gr + battlebest
                      + ND
                      + Atcat + unem.y
                      + bcratio + p2sq,
                      data = piracy))


#Linear Probability Model
summary(LPM3 <- lm(incbinary ~ cmsq + pop.gr + battlebest
                   + ND
                   + Atcat + unem.y
                   + bcratio + p2sq,
                   data = piracy))

#Logit model
summary(L3 <- glm(incbinary ~ cmsq + pop.gr + battlebest
                  + ND
                  + Atcat + unem.y
                  + bcratio + p2sq,
                  data = piracy, family = "binomial"))

#Negative Binomial model
summary(NB3 <- glm.nb(incidents ~ cmsq + pop.gr + battlebest
                      + ND
                      + Atcat + unem.y
                      + bcratio + p2sq,
                      data = piracy))

#Linear Probability Model
summary(LPM4 <- lm(incbinary ~ cmsq + pop.gr + battlebest
                   + ND
                   + mobilep100 + unem.y
                   + bcratio + p2sq,
                   data = piracy))

#Logit model
summary(L4 <- glm(incbinary ~ cmsq + pop.gr + battlebest
                  + ND
                  + mobilep100 + unem.y
                  + bcratio + p2sq,
                  data = piracy, family = "binomial"))

#Negative Binomial model
summary(NB4 <- glm.nb(incidents ~ cmsq + pop.gr + battlebest
                      + ND
                      + mobilep100 + unem.y
                      + bcratio + p2sq,
                      data = piracy))

odTest(NB1)
odTest(NB2)
odTest(NB3)
odTest(NB4)

