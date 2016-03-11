########################
# Maritime jack - Descriptive Statistics
#######################
library(ggplot2)
library(sjPlot)
library(rworldmap)
library(rworldxtra)
library(reshape2)

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd(""),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

jack <- read.csv("JackSparrow.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = c("", "NA"))
jack$X <- NULL
jack[1101:1122, ] #Kazakhstan, landlocked: no access to the high sea
jack <- jack[-c(1101:1122), ]

#ten <- read.csv("MaritimejackTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE, na.strings = c("", "NA"))

# Dynamically Link to first R script file
#source("MergeAll.R")

#############################################
#Eyeballing the dependent variable: incidents
#############################################
summary(jack$incidents)
a <- mean(jack$incidents)
b <- var(jack$incidents)
sum(a)
sum(b)
sum(a-b)
mytable <- table(jack$incidents)
list(mytable)
prop.table(mytable)
summary(jack$country)
sum(jack$incidents)


sjp.setTheme(theme = "scatter",
             geom.label.size = 2.5,
             geom.label.color = "navy",
             axis.textsize = .8,
             axis.title.size = .9)
sjp.frq(jack$incidents,
        title = "Fig. 1 - Frequency of Pirate Attacks 1993-2014",
        geom.colors = "darkorange",
        sort.frq = "none",
        axisTitle.x = "Incidents of Maritime Piracy",
        axisTitle.y = "Country-years",
        showPercentageValues = FALSE,
        coord.flip = FALSE)


Africa <- jack[which(jack$continent=="Africa"),]
summary(Africa$incidents)
table(Africa$incidents)
mean(Africa$incidents)
var(Africa$incidents)

MENA <- jack[which(jack$continent=="MENA"),]
table(MENA$incidents)
summary(MENA$incidents)
mean(MENA$incidents)
var(MENA$incidents)

SAsia <- jack[which(jack$continent=="SouthAsia"),]
table(SAsia$incidents)
summary(SAsia$incidents)
mean(SAsia$incidents)
var(SAsia$incidents)

EAsia <- jack[which(jack$continent=="EastAsia"),]
table(EAsia$incidents)
summary(EAsia$incidents)
mean(EAsia$incidents)
var(EAsia$incidents)

Asia <- jack[which(jack$continent=="EastAsia"|jack$continent=="SouthAsia"),]
table(Asia$incidents)
summary(Asia$incidents)
mean(Asia$incidents)
var(Asia$incidents)

ROW <- jack[which(jack$continent=="ROW"),]
table(ROW$incidents)
summary(ROW$incidents)
mean(ROW$incidents)
var(ROW$incidents)

sum(jack$incidents)
Afsum <- sum(Africa$incidents)
MEsum <- sum(MENA$incidents)
Assum <- sum(Asia$incidents)
ROWsum <- sum(ROW$incidents)
sum(Afsum + MEsum + Assum + ROWsum)

sjp.setTheme(theme = "scatter",
             geom.label.size = 2.5,
             geom.label.color = "navy",
             axis.textsize = .8,
             axis.title.size = .9)
sjp.frq(Africa$incidents,
        title = "Fig. 2a - Africa 1993-2014",
        geom.colors = "firebrick1",
        sort.frq = "none",
        axisTitle.x = "Incidents of Maritime Piracy",
        axisTitle.y = "Country-years",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(MENA$incidents,
        title = "Fig. 2b - MENA 1993-2014",
        geom.colors = "chocolate4",
        sort.frq = "none",
        axisTitle.x = "Incidents of Maritime Piracy",
        axisTitle.y = "Country-years",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(Asia$incidents,
        title = "Fig. 2c - Asia 1993-2014",
        geom.colors = "mediumspringgreen",
        sort.frq = "none",
        axisTitle.x = "Incidents of Maritime Piracy",
        axisTitle.y = "Country-years",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(ROW$incidents,
        title = "Fig. 2d - ROW 1993-2014",
        geom.colors = "deepskyblue2",
        sort.frq = "none",
        axisTitle.x = "Incidents of Maritime Piracy",
        axisTitle.y = "Country-years",
        showPercentageValues = FALSE,
        coord.flip = FALSE)

farben = c("firebrick1", "chocolate4", "mediumspringgreen", "deepskyblue2")

slices <- c(Afsum, MEsum, Assum, ROWsum) 
lbls <- c("Africa", "MENA", "Asia", "ROW")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=farben,
    main="Fig. 3 - Shares of Incidents of Maritime Piracy 1993-2014")

jack$continent2 <- jack$continent
jack$incidents2 <- jack$incidents
jack$continent2 <- as.character(jack$continent2)
jack$continent2[which(jack$continent=="EastAsia")] <- "Asia"
jack$continent2[which(jack$continent=="SouthAsia")] <- "Asia"
jack$continent2 <- as.factor(jack$continent2)
aggrtpi <- dcast(jack, continent2 + year ~ incidents, sum) #p317 R for Dummies
aggrtpi$ytotal <- rowSums(aggrtpi[,3:67])

p <- ggplot(data = aggrtpi, aes(x = year, y = ytotal, group = continent2, color = continent2)) + geom_line() + ggtitle("Fig. 4 - Incidents of Maritime Piracy 1993-2014") + labs(x = "Year", y = "No. of Incidents")
p + scale_colour_discrete(name  ="Region", labels=c("Africa", "Asia", "MENA", "ROW"))

aggrtc <- dcast(jack, iso2c ~ incidents, sum) #p317 R for Dummies
aggrtc$ctotal <- rowSums(aggrtc[,2:66])
#q <- ggplot(data = aggrtc, aes(x = country, y = ctotal, group = country, color = country))
#q + ggtitle("Fig. 5 - Total No. of Incidents per Country") + labs(x = "Country", y = "No. of Incidents")

aggrtc$iso2c <- as.factor(aggrtc$iso2c)
aggrtc$category <- aggrtc$ctotal
aggrtc$category <- cut(aggrtc$ctotal, c(0,1,2,3,10,20,50,2000))

d <- data.frame(country=c(aggrtc$iso2c),
                value=aggrtc$category)
n <- joinCountryData2Map(d,
                         joinCode="ISO2", 
                         nameJoinColumn="country",
                         mapResolution = "high")

mapCountryData(n, 
               nameColumnToPlot="value", 
               mapTitle="Fig. 5a - Total Incidents per Country 1993-2014",
               mapRegion="Africa",
               catMethod="categorical",
               colourPalette=c('burlywood1', 'lightgreen', 'darkgreen', 'yellow', 'orange', 'pink', 'darkred'),
               addLegend=FALSE,
               borderCol="black",
               missingCountryCol="white",
               oceanCol = "lightblue")
mapCountryData(n, 
               nameColumnToPlot="value",
               mapTitle="Fig. 5b - Total Incidents per Country 1993-2014",
               mapRegion="Asia",
               catMethod="categorical",
               colourPalette=c('burlywood1', 'lightgreen', 'darkgreen', 'yellow', 'orange', 'red'),
               addLegend=FALSE,
               borderCol="black",
               missingCountryCol="white",
               oceanCol = "lightblue")
mapCountryData(n, 
               nameColumnToPlot="value", 
               mapTitle="Fig. 5c - Total Incidents per Country 1993-2014",
               mapRegion="Latin America",
               catMethod="categorical",
               colourPalette=c('burlywood1', 'lightgreen', 'darkgreen', 'yellow', 'orange', 'red'),
               addLegend=FALSE,
               borderCol="black",
               missingCountryCol="white",
               oceanCol = "lightblue")



summary(NB1 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel))
summary(Ptest <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel, family = "poisson"))
X <- 2*(logLik(NB1) - logLik(Ptest))
list(X)
pchisq(X, df = 1, lower.tail=FALSE)

rm(NB1, Ptest, X)
