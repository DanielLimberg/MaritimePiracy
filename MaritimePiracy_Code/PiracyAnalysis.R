########################
# Maritime Piracy - Analysis
#######################

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd(""),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

piracy <- read.csv("piracy.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = c("", "NA"))
piracy$X <- NULL

# Dynamically Link to first R script file
#source("MergeAll.R")

summary(piracy$iso2c)
class(piracy$iso2c)
piracy$iso2c <- as.character(piracy$iso2c)
piracy$iso2c[is.na(piracy$iso2c)] <- "NA"

piracy$cyr <- as.character(paste(piracy$iso2c, piracy$year, sep = "-"))


#Logit model
summary(logit1 <- glm(incbinary ~ cmort + corruption + pop.gr + battlelow
                      + FD + SD + ED + DD
                      + GDPpc + WTI + unem.total
                      + log(coastkm) + ((polity2)^2)
                      + as.factor(country) + as.factor(year),
                      data = piracy, family = "binomial"))
summary(logit2 <- glm(incbinary ~ cmort +  corruption + pop.gr + battlelow
                      + FD + SD + ED + DD
                      + GNIpc + WTI + unem.total
                      + log(coastkm) + ((polity2)^2)
                      + as.factor(country) + as.factor(year),
                      data = piracy, family = "binomial"))
summary(logit3 <- glm(incbinary ~ cmort + corruption + pop.gr + battlelow
                      + FD + SD + ED + DD 
                      + GNIpc.group + WTI + unem.total
                      + log(coastkm) + ((polity2)^2)
                      + as.factor(country) + as.factor(year),
                      data = piracy, family = "binomial"))
summary(logit3 <- glm(incbinary ~ cmort + corruption + pop.gr + battlelow
                      + FD + SD + ED + DD
                      + GNIpc.group + WTI + unem.total
                      + log(coastkm) + ((polity2)^2)
                      + as.factor(country) + as.factor(year),
                      data = piracy, family = "binomial"))


#Neg. Bin. model
summary(NB1 <- glm.nb(incidents ~ cmort + corruption + pop.gr + battlelow
                      + FD + SD + ED + DD
                      + GDPpc + WTI + unem.total
                      + log(coastkm) + ((polity2)^2)
                      + as.factor(country) + as.factor(year),
                      data = piracy))
summary(NB2 <- glm.nb(incidents ~ cmort + corruption + pop.gr + battlelow
                      + FD + SD + ED + DD
                      + GNIpc + WTI + unem.total
                      + log(coastkm) + ((polity2)^2)
                      + as.factor(country) + as.factor(year),
                      data = piracy))
summary(NB3 <- glm.nb(incidents ~ cmort + corruption + pop.gr + battlelow
                      + FD + SD + ED + DD
                      + GNIpc + WTI + unem.total
                      + log(coastkm) + ((polity2)^2)
                      + as.factor(country) + as.factor(year),
                      data = piracy))
