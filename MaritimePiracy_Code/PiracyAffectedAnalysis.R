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

#jack$region <- jack$continent
#jack$region <- as.character(jack$region)
#jack$region[which(jack$continent=="SouthAsia")] <- 'Asia'
#jack$region[which(jack$continent=="EastAsia")] <- 'Asia'
#jack$region <- as.factor(jack$region)
jack <- jack[-c(1101:1122), ]

Asia <- subset(jack, region == "Asia")
write.csv(jack, file = "jackAsia.csv")
ROW <- subset(jack, region == "MENA" | region == "Africa" | region == "ROW")
write.csv(ROW, file = "jackMENAROWAfrica.csv")

jack$bcratio <- (jack$borderkm/jack$coastkm)
jack$p2sq <- ((jack$polity2)^2)
jack$cmsq <- ((jack$cmort)^2)

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

#Linear Probability Model
summary(LPM2 <- lm(incbinary ~ cmort + pop.gr + battlebest
                   + ND
                   + Atcat + unem.y
                   + bcratio + p2sq,
                   data = jack))

#Logit model
summary(L2 <- glm(incbinary ~ cmort + pop.gr + battlebest
                  + ND
                  + Atcat + unem.y
                  + bcratio + p2sq,
                  data = jack, family = "binomial"))

#Negative Binomial model
summary(NB2 <- glm.nb(incidents ~ cmort + pop.gr + battlebest
                      + ND
                      + Atcat + unem.y
                      + bcratio + p2sq,
                      data = jack))

#Linear Probability Model
summary(LPM3 <- lm(incbinary ~ cmsq + pop.gr + battlebest
                   + ND
                   + Atcat + unem.y
                   + bcratio + p2sq,
                   data = jack))

#Logit model
summary(L3 <- glm(incbinary ~ cmsq + pop.gr + battlebest
                  + ND
                  + Atcat + unem.y
                  + bcratio + p2sq,
                  data = jack, family = "binomial"))

#Negative Binomial model
summary(NB3 <- glm.nb(incidents ~ cmsq + pop.gr + battlebest
                      + ND
                      + Atcat + unem.y
                      + bcratio + p2sq,
                      data = jack))

#Linear Probability Model
summary(LPM4 <- lm(incbinary ~ cmsq + pop.gr + battlebest
                   + ND
                   + mobilep100 + unem.y
                   + bcratio + p2sq,
                   data = jack))

#Logit model
summary(L4 <- glm(incbinary ~ cmsq + pop.gr + battlebest
                  + ND
                  + mobilep100 + unem.y
                  + bcratio + p2sq,
                  data = jack, family = "binomial"))

#Negative Binomial model
summary(NB4 <- glm.nb(incidents ~ cmsq + pop.gr + battlebest
                      + ND
                      + mobilep100 + unem.y
                      + bcratio + p2sq,
                      data = jack))
