########################
# Maritime Piracy - Analysis
#######################

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd(""),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

piracy <- read.csv("piracy.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = c("", "NA"))
piracy$X <- NULL
piracy$bcratio <- (piracy$borderkm/piracy$coastkm)
piracy$p2sq <- ((piracy$polity2)^2)

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
