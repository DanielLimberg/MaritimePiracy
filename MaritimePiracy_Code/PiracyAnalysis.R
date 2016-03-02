########################
# Maritime Piracy - Analysis
#######################

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd(""),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Code"),silent=TRUE)
getwd()

# Dynamically Link to first R script file
source("MergeAll.R")


#Logit model
summary(logit1 <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel, family = "binomial"))
summary(logit2 <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GNIpc + WTI + unem.total + log(coastkm) + battlelow, data = panel, family = "binomial"))
summary(logit3 <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GNIpc + WTI + unem.y.m + log(coastkm) + battlelow, data = panel, family = "binomial"))
summary(logit4 <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GNIpc + WTI + unem.y.m + (borderkm/coastkm) + battlelow, data = panel, family = "binomial"))
summary(logit5 <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GNIpc + WTI + unem.y.m + (borderkm/coastkm) + battlelow, data = panel, family = "binomial"))
summary(logit6 <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + log(GDP) + WTI + unem.y.m + (borderkm/coastkm) + battlelow + as.factor(country), data = panel, family = "binomial"))
summary(logit7 <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + log(mobile) + WTI + unem.y.m + (borderkm/coastkm) + battlelow + as.factor(country), data = panel, family = "binomial"))


#Neg. Bin. model
summary(NB1 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel))
summary(NB2 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GNIpc + WTI + unem.total + log(coastkm) + battlelow, data = panel))
summary(NB3 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GNIpc + WTI + unem.y.m + log(coastkm) + battlelow, data = panel))
summary(NB4 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GNIpc + WTI + unem.y.m + (borderkm/coastkm) + battlelow, data = panel))
summary(NB5 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GNIpc + WTI + unem.y.m + (borderkm/coastkm) + battlelow + as.factor(country), data = panel))
summary(NB6 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + log(GDP) + WTI + unem.y.m + (borderkm/coastkm) + battlelow + as.factor(country), data = panel))
summary(NB7 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + log(mobile) + WTI + unem.y.m + (borderkm/coastkm) + battlelow + as.factor(country), data = panel))
