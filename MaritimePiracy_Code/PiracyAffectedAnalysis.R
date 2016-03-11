########################
# Maritime Piracy - Analysis
#######################
library(aod)
library(MASS)

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd(""),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

jack <- read.csv("JackSparrow.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = c("", "NA"))
jack$X <- NULL
jack[1101:1122, ] #Kazakhstan, landlocked: no access to the high sea
jack <- jack[-c(1101:1122), ]
jack$bcratio <- (jack$borderkm/jack$coastkm)
jack$p2sq <- ((jack$polity2)^2)

#Linear Probability Model
summary(LPM1 <- lm(incbinary ~ cmort + pop.gr + battlebest
          + ND
          + Atcat + unem.total
          + bcratio + p2sq,
          data = jack))

#Logit model
summary(L1 <- glm(incbinary ~ cmort + pop.gr + battlebest
           + ND
           + Atcat + unem.total
           + bcratio + p2sq,
           data = jack, family = "binomial"))

#Negative Binomial model
summary(NB1 <- glm.nb(incidents ~ cmort + pop.gr + battlebest
                  + ND
                  + Atcat + unem.total
                  + bcratio + p2sq,
                  data = jack))
