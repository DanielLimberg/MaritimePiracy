########################
# Maritime Asia - Analysis
#######################
library(aod)
library(MASS)
library(plm)

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd(""),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

piracy <- read.csv("piracy.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = c("", "NA"))
piracy$X <- NULL
#piracy$bcratio <- (piracy$borderkm/piracy$coastkm)
#piracy$p2sq <- ((piracy$polity2)^2)
#piracy$cmsq <- ((piracy$cmort)^2)

piracy$region <- piracy$continent
piracy$region <- as.character(piracy$region)
piracy$region[which(piracy$continent=="SouthAsia")] <- 'Asia'
piracy$region[which(piracy$continent=="EastAsia")] <- 'Asia'
piracy$region <- as.factor(piracy$region)

#Asia <- subset(piracy, region == "Asia")
#write.csv(Asia, file = "piracyAsia.csv")
#ROW <- subset(piracy, region == "MENA" | region == "Africa" | region == "ROW")
#write.csv(ROW, file = "piracyMENAROWAfrica.csv")

summary(Asia$incidents)

hist(Asia$incidents,
     main="Frequency Distribution", 
     xlab="Incidents of Maritime Asia in Asia",
     ylab="Frequency",
     border="blue",
     col="green",
     xlim=c(0,150),
     las=1, 
     breaks=10)

#Linear Probability Model
summary(LPM1 <- lm(incbinary ~ cmort + pop.gr + battlebest
                   + ND
                   + Atcat + unem.total
                   + bcratio + p2sq,
                   data = Asia))

#Logit model
summary(L1 <- glm(incbinary ~ cmort + pop.gr + battlebest
                  + ND
                  + Atcat + unem.total
                  + bcratio + p2sq,
                  data = Asia, family = "binomial"))

#Negative Binomial model
summary(NB1 <- glm.nb(incidents ~ cmort + pop.gr + battlebest
                      + ND
                      + Atcat + unem.total
                      + bcratio + p2sq,
                      data = Asia))

#Linear Probability Model
summary(LPM2 <- lm(incbinary ~ cmort + pop.gr + battlebest
                   + ND
                   + Atcat + unem.y
                   + bcratio + p2sq,
                   data = Asia))

#Logit model
summary(L2 <- glm(incbinary ~ cmort + pop.gr + battlebest
                  + ND
                  + Atcat + unem.y
                  + bcratio + p2sq,
                  data = Asia, family = "binomial"))

#Negative Binomial model
summary(NB2 <- glm.nb(incidents ~ cmort + pop.gr + battlebest
                      + ND
                      + Atcat + unem.y
                      + bcratio + p2sq,
                      data = Asia))


#Linear Probability Model
summary(LPM3 <- lm(incbinary ~ cmsq + pop.gr + battlebest
                   + ND
                   + Atcat + unem.y
                   + bcratio + p2sq,
                   data = Asia))

#Logit model
summary(L3 <- glm(incbinary ~ cmsq + pop.gr + battlebest
                  + ND
                  + Atcat + unem.y
                  + bcratio + p2sq,
                  data = Asia, family = "binomial"))

#Negative Binomial model
summary(NB3 <- glm.nb(incidents ~ cmsq + pop.gr + battlebest
                      + ND
                      + Atcat + unem.y
                      + bcratio + p2sq,
                      data = Asia))

#Linear Probability Model
summary(LPM4 <- lm(incbinary ~ cmsq + pop.gr + battlebest
                   + ND
                   + mobilep100 + unem.y
                   + bcratio + p2sq,
                   data = Asia))

#Logit model
summary(L4 <- glm(incbinary ~ cmsq + pop.gr + battlebest
                  + ND
                  + mobilep100 + unem.y
                  + bcratio + p2sq,
                  data = Asia, family = "binomial"))

#Negative Binomial model
summary(NB4 <- glm.nb(incidents ~ cmsq + pop.gr + battlebest
                      + ND
                      + mobilep100 + unem.y
                      + bcratio + p2sq,
                      data = Asia))

