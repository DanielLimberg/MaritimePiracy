########################
# Maritime Piracy - Analysis
#######################

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracyWrangling/Data"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

# Dynamically Link to first R script file
source("MergeAll.R")

#Logit model 1a
summary(logit1 <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + coastkm + battlelow, data = panel, family = "binomial"))
summary(logit1 <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel, family = "binomial"))
summary(logit2 <- glm(incbinary ~ cmort + FD + DD + poprur.gr + ((polity2)^2) + GNIpc + WTI + unem.y.m + coastkm + battlelow, data = panel, family = "binomial"))

#Neg. Bin. model 1a
summary(ND1 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + coastkm + battlelow, data = panel))
summary(ND1 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel))
summary(ND2 <- glm.nb(incbinary ~ cmort + FD + DD + poprur.gr + ((polity2)^2) + GNIpc + WTI + unem.y.m + coastkm + battlelow, data = panel))
