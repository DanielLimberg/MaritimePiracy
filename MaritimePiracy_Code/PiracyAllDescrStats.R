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
        title = "Fig. 1 - Frequency of Pirate Attacks 1993-2014",
        geom.colors = "darkorange",
        sort.frq = "desc",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
        showPercentageValues = FALSE,
        coord.flip = FALSE)

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

summary(NB1 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel))
summary(Ptest <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel, family = "poisson"))
X <- 2*(logLik(NB1) - logLik(Ptest))
list(X)
pchisq(X, df = 1, lower.tail=FALSE)

rm(NB1, Ptest, X)


Africa <- filter(piracy, continent == "Africa")
summary(Africa$incidents)
table(Africa$incidents)
mean(Africa$incidents)
var(Africa$incidents)
MENA <- filter(piracy, continent == "MENA")
table(MENA$incidents)
summary(MENA$incidents)
mean(MENA$incidents)
var(MENA$incidents)
SAsia <- filter(piracy, continent == "SouthAsia")
table(SAsia$incidents)
summary(SAsia$incidents)
mean(SAsia$incidents)
var(SAsia$incidents)
EAsia <- filter(piracy, continent == "EastAsia")
table(EAsia$incidents)
summary(EAsia$incidents)
mean(EAsia$incidents)
var(EAsia$incidents)
ROW <- filter(piracy, continent == "ROW")
table(ROW$incidents)
summary(ROW$incidents)
mean(ROW$incidents)
var(ROW$incidents)

sjp.setTheme(theme = "scatter",
             geom.label.size = 2.5,
             geom.label.color = "navy",
             axis.textsize = .8,
             axis.title.size = .9)
sjp.frq(Africa$incidents,
        title = "Africa - Frequency of Pirate Attacks '93-'14",
        geom.colors = "firebrick1",
        sort.frq = "desc",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(MENA$incidents,
        title = "MENA - Frequency of Pirate Attacks '93-'14",
        geom.colors = "chocolate4",
        sort.frq = "desc",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(SAsia$incidents,
        title = "S.Asia - Frequency of Pirate Attacks '93-'14",
        geom.colors = "mediumspringgreen",
        sort.frq = "desc",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(EAsia$incidents,
        title = "E.Asia - Frequency of Pirate Attacks '93-'14",
        geom.colors = "yellow2",
        sort.frq = "desc",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(ROW$incidents,
        title = "ROW - Frequency of Pirate Attacks '93-'14",
        geom.colors = "deepskyblue2",
        sort.frq = "desc",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
        showPercentageValues = FALSE,
        coord.flip = FALSE)

par(mfrow=c(1,2))
hist(piracy$incidents[piracy$continent=="Africa"],
     col=(c("firebrick1")),
     breaks=50,
     main="Piracy in Africa",
     xlab="Incidents of Piracy", ylab="Frequency")
hist(piracy$incidents[piracy$continent=="MENA"],
     col=(c("chocolate4")),
     breaks=50,
     main="Piracy in MENA Region",
     xlab="Incidents of Piracy", ylab="Frequency")
hist(piracy$incidents[piracy$continent=="SouthAsia"],
     col=(c("mediumspringgreen")),
     breaks=50,
     main="Piracy in South Asia",
     xlab="Incidents of Piracy", ylab="Frequency")
hist(piracy$incidents[piracy$continent=="EastAsia"],
     col=(c("yellow")),
     breaks=50,
     main="Piracy in East Asia",
     xlab="Incidents of Piracy", ylab="Frequency")
par(mfrow=c(1,1))
hist(piracy$incidents[piracy$continent=="ROW"],
     col=(c("deepskyblue2")),
     breaks=50,
     main="Piracy in ROW",
     xlab="Incidents of Piracy", ylab="Frequency")

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