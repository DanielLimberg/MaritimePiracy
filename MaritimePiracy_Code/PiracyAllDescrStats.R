########################
# Maritime Piracy - Descriptive Statistics
#######################

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd(""),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

piracy <- read.csv("piracy.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = c("", "NA"))
piracy$X <- NULL

# Dynamically Link to first R script file
#source("MergeAll.R")


#############################################
#Eyeballing the dependent variable: incidents
#############################################
summary(piracy$incidents)
var(piracy$incidents)
mytable <- table(piracy$incidents)
list(mytable)
prop.table(mytable)
summary(piracy$country)
list(piracy$country)
library(sjPlot)
sjp.setTheme(theme = "scatter",
             geom.label.size = 2.5,
             geom.label.color = "navy",
             axis.textsize = .8,
             axis.title.size = .9)
sjp.frq(piracy$incidents,
        title = "Fig. 1 - Frequency of Pirate Attacks",
        geom.colors = "darkorange",
        sort.frq = "desc",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
        coord.flip = FALSE)
#sjp.setTheme(theme = "scatter",
#             geom.label.size = 2.5,
#             geom.label.color = "navy",
#             axis.textsize = .8,
#             axis.title.size = .9)
#sjp.frq(piracy$incidents,
#        type = "hist",
#        title = "Fig. 1 - Frequency of Pirate Attacks",
#        geom.colors = "darkorange",
#        sort.frq = "desc",
#        axisTitle.x = "Incidents of Piracy per country-year",
#        axisTitle.y = "Frequency",
#        geom.size = 3,
#        showPercentageValues = FALSE,
#        showStandardNormalCurve = TRUE,
#        normalCurveColor = "red1",
#        normalCurveSize = 1,
#        normalCurveAlpha = 1,
#        coord.flip = FALSE)


year = piracy$year #x-axis
incidents = piracy$incidents #y-axis
plot(year, incidents, type="l",
     xlab="years", ylab="Incidents of Maritime Piracy", main="Maritime Piracy 1993 - 2014",
     col="darkorange",
     lwd=2)

temp <- filter(piracy, piracy$incidents >=32)
hist(temp$incidents, main="Incidents of Piracy", col="orange", ylim=c(1,400), xlim=c(0,30))
table(temp$incidents)
rm(temp)



############################################
#Plot some explenatory var against incidents
############################################
table(merge8$cmort)
summary(merge8$cmort)
table(panel$cmort, panel$country)

#compare mean and variance
mean(panel$incidents)
var(panel$incidents)
tapply(panel$incidents, panel$continent, mean)
tapply(panel$incidents, panel$continent, var)
#variance always larger than mean

hist(panel$incidents, main="Incidents of Piracy", col="blue", breaks = 100) #poisson distribution
summary(panel$incidents) #mean = 1.725
library(plm)
panel <- pdata.frame(piracy, index=c("iso2c", "year")) #setting dataframe to panel data
var(panel$incidents) #variance = 67.684
var(piracy$incidents)

#African countries only
Africa <- filter(panel, continent == "Africa")
hist(Africa$incidents, main="Incidents of Piracy", col="red", breaks = 100) #poisson distribution
summary(Africa$incidents) #mean = 1.674
var(Africa$incidents) #variance = 21.52207
#Asian countries only
Asia <- filter(panel, continent == "Asia")
hist(Asia$incidents, main="Incidents of Piracy", col="yellow", breaks = 100) #poisson distribution
summary(Asia$incidents) #mean = 9.315
var(Asia$incidents) #variance = 513.1568
rm(Africa, Asia)

summary(NB1 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel))
summary(Ptest <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel, family = "poisson"))
X <- 2*(logLik(NB1) - logLik(Ptest))
list(X)
pchisq(X, df = 1, lower.tail=FALSE)

rm(NB1, Ptest, X)

hist(panel$incidents,
     col=(c("goldenrod1")),
     breaks=50,
     main="Piracy aroud the World",
     xlab="Incidents of Piracy", ylab="Frequency")
hist(panel$incidents,
     col=(c("goldenrod1")),
     breaks=100,
     main="Piracy aroud the World",
     xlab="Incidents of Piracy", ylab="Frequency")


par(mfrow=c(1,3))
hist(panel$incidents[panel$continent=="Africa"],
     col=(c("firebrick1")),
     breaks=10,
     main="Piracy in Africa",
     xlab="Incidents of Piracy", ylab="Frequency")
hist(panel$incidents[panel$continent=="Asia"],
     col=(c("mediumspringgreen")),
     breaks=10,
     main="Piracy in Asia",
     xlab="Incidents of Piracy", ylab="Frequency")
hist(panel$incidents[panel$continent=="ROW"],
     col=(c("deepskyblue2")),
     breaks=10,
     main="Piracy in ROW",
     xlab="Incidents of Piracy", ylab="Frequency")
par(mfrow=c(1,1))

plot(panel$year, panel$incidents, type = "b",
     col=(c("goldenrod1")),
     main = "Maritime Piracy 1993 - 2014", 
     xlab = "Years", 
     ylab = "Incidents of Piracy")

hist(panel$lowfatalityestimate,
     col=(c("darkmagenta")),
     breaks=100,
     main="Battle related deaths (low)",
     xlab="Death Count", ylab="Frequency")

library(ggplot2)
ggplot(aes(x = year, y = incidents), col="goldenrod1", data = piracy) + geom_line()
#ggplot(aes(x = year, y = incidents), data = merge6) + geom_line()
#scatterplot(incidents~year, col="goldenrod1", boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=panel)

cor.test(merge8$GDP, merge8$mobile)
cor.test(merge8$GNIpc, merge8$mobilep100)

#################
#GDPpc and piracy
#################
GDP = panel$GDPpc #x-axis
incidents = panel$incidents #y-axis
plot(GDP, incidents, xlab="GDP per capita", ylab="Incidents of Piracy")

######################
#GDP growth and piracy
######################
GDP = panel$GDP.gr #x-axis
incidents = panel$incidents #y-axis
plot(GDP, incidents, xlab="GDP growth", ylab="Incidents of Piracy")

########################
#Mobile phones and GDPpc
########################
mobile = panel$mobile #x-axis
GDP = panel$GDPpc #y-axis
plot(mobile, GDP, xlab="Mobile Phones", ylab="GDP per capita")

#############################
#Mobile phones and GDP growth
#############################
mobile = panel$mobile #x-axis
GDP = panel$GDP.gr #y-axis
plot(mobile, GDP, xlab="Mobile Phones", ylab="GDP growth")

################################
#Mobile Phones per 100 and GDPpc
################################
mobile = panel$mobilep100 #x-axis
GDP = panel$GDPpc #y-axis
plot(mobile, incidents, xlab="Mobile Phones per 100", ylab="GDP per capita")

#########################
#Mobile phones and piracy
#########################
mobile = panel$mobile #x-axis
incidents = panel$incidents #y-axis
plot(mobile, incidents, xlab="Mobile Phones", ylab="Incidents of Piracy")

#################################
#Mobile Phones per 100 and piracy
#################################
mobile = panel$mobilep100 #x-axis
incidents = panel$incidents #y-axis
plot(mobile, incidents, xlab="Mobile Phones per 100", ylab="Incidents of Piracy")

###################
#Drought and piracy
###################
drought = panel$Drought #x-axis
incidents = panel$incidents #y-axis
plot(drought, incidents, xlab="Drought", ylab="Incidents of Piracy")
drought = panel$DD #x-axis
incidents = panel$incidents #y-axis
plot(drought, incidents, xlab="Drought", ylab="Incidents of Piracy")

#################
#Flood and piracy
#################
flood = panel$Flood #x-axis
incidents = panel$incidents #y-axis
plot(flood, incidents, xlab="Flood", ylab="Incidents of Piracy")
flood = panel$FD #x-axis
incidents = panel$incidents #y-axis
plot(flood, incidents, xlab="Flood", ylab="Incidents of Piracy")

################
#GINI and piracy
################
gini = panel$gini #x-axis
incidents = panel$incidents #y-axis
plot(gini, incidents, xlab="GINI index", ylab="Incidents of Piracy")

################
#Oil prices and piracy
################
WTI = panel$WTI #x-axis
incidents = panel$incidents #y-axis
plot(WTI, incidents, xlab="W.T.I., annual average", ylab="Incidents of Piracy")

rm(drought, flood, GDP, incidents, mobile, gini, WTI)



temp <- filter(panel, incidents != 0) #only 804 observations left if all 0 deleted
#################
#GDPpc and piracy
#################
GDP = temp$GDPpc #x-axis
incidents = temp$incidents #y-axis
plot(GDP, incidents, xlab="GDP per capita", ylab="Incidents of Piracy")

######################
#GDP growth and piracy
######################
GDP = temp$GDP.gr #x-axis
incidents = temp$incidents #y-axis
plot(GDP, incidents, xlab="GDP growth", ylab="Incidents of Piracy")

########################
#Mobile phones and GDPpc
########################
mobile = temp$mobile #x-axis
GDP = temp$GDPpc #y-axis
plot(mobile, GDP, xlab="Mobile Phones", ylab="GDP per capita")

#############################
#Mobile phones and GDP growth
#############################
mobile = temp$mobile #x-axis
GDP = temp$GDP.gr #y-axis
plot(mobile, GDP, xlab="Mobile Phones", ylab="GDP growth")

################################
#Mobile Phones per 100 and GDPpc
################################
mobile = temp$mobilep100 #x-axis
GDP = temp$GDPpc #y-axis
plot(mobile, incidents, xlab="Mobile Phones per 100", ylab="GDP per capita")

#####################################
#Mobile phones per 100 and GDP growth
#####################################
mobile = temp$mobilep100 #x-axis
GDP = temp$GDP.grow #y-axis
plot(mobile, GDP, xlab="Mobile Phones", ylab="GDP growth")

#########################
#Mobile phones and piracy
#########################
mobile = temp$mobile #x-axis
incidents = temp$incidents #y-axis
plot(mobile, incidents, xlab="Mobile Phones", ylab="Incidents of Piracy")

#################################
#Mobile Phones per 100 and piracy
#################################
mobile = temp$mobilep100 #x-axis
incidents = temp$incidents #y-axis
plot(mobile, incidents, xlab="Mobile Phones per 100", ylab="Incidents of Piracy")

###################
#Drought and piracy
###################
drought = temp$Drought #x-axis
incidents = temp$incidents #y-axis
plot(drought, incidents, xlab="Drought", ylab="Incidents of Piracy")
drought = temp$DD #x-axis
incidents = temp$incidents #y-axis
plot(drought, incidents, xlab="Drought", ylab="Incidents of Piracy")

#################
#Flood and piracy
#################
flood = temp$Flood #x-axis
incidents = temp$incidents #y-axis
plot(flood, incidents, xlab="Flood", ylab="Incidents of Piracy")
flood = temp$FD #x-axis
incidents = temp$incidents #y-axis
plot(flood, incidents, xlab="Flood", ylab="Incidents of Piracy")

################
#GINI and piracy
################
gini = temp$gini #x-axis
incidents = temp$incidents #y-axis
plot(gini, incidents, xlab="GINI index", ylab="Incidents of Piracy")

rm(drought, flood, GDP, incidents, mobile, temp, gini)