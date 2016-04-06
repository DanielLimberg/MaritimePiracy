########################
# Maritime Piracy - Descriptive Statistics
#######################
library(ggplot2)
library(sjPlot)
library(rworldmap)
library(rworldxtra)
library(reshape2)
library(countrycode)

#set working directories if necessary (if data lies in git repo it is not necessary though)
<<<<<<< HEAD
try(setwd(""),silent=TRUE)
=======
try(setwd("E:/bjoer/Documents/DatasetsBackup/04_SpringSemester/MaritimePiracy_Data"),silent=TRUE)
>>>>>>> 0323bade769ea616e8eaa015aedcb9804d9e43e6
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE, na.strings = c("", "NA"))

# Dynamically Link to first R script file
#source("MergeAll.R")

############################
#Piracy data from Tenneessee
############################
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))

#rename
names(shipping)[6] <- 'time' #former var name: timeofdayrecode
shipping$time <- factor(shipping$time,
                        levels = c(1,2,3,4),
                        labels = c("beforenoon", "afternoon", "night", "morning"))
shipping$time <- factor(shipping$time)

names(shipping)[18] <- 'vessel' #former var name: vessel_type
shipping$vessel[shipping$vessel==2] <- 111
shipping$vessel[shipping$vessel==4] <- 111
shipping$vessel[shipping$vessel==6] <- 111
shipping$vessel[shipping$vessel==3] <- 222
shipping$vessel[shipping$vessel==7] <- 222
shipping$vessel[shipping$vessel==8] <- 222
shipping$vessel[shipping$vessel==1] <- 333
shipping$vessel[shipping$vessel==5] <- 333
shipping$vessel[shipping$vessel==9] <- 333
shipping$vessel[shipping$vessel==10] <- 333
shipping$vessel[shipping$vessel==-99] <- NA
shipping$vessel[shipping$vessel==22] <- NA
shipping$vessel[shipping$vessel==696] <- NA
shipping$vessel[shipping$vessel==111] <- 1
shipping$vessel[shipping$vessel==222] <- 2
shipping$vessel[shipping$vessel==333] <- 3
shipping$vessel <- factor(shipping$vessel,
                          levels = c(1,2,3),
                          labels = c("merchant", "oil", "other"))

names(shipping)[24] <- 'incidents' #former var name: Incident_type_recode
shipping$incidents2 <- shipping$incidents

#no differentiation between unsucc and succ attacks
shipping$incidents[shipping$incidents==-99] <- NA
shipping$incidents[is.na(shipping$incidents)] <- 0
#shipping$incidents <- factor(shipping$incidents,
#                             levels = c(1),
#                             labels = c("incidents", "incidents"))
shipping$incidents[shipping$incidents==0] <- 1
shipping$incidents[shipping$incidents==1] <- "incidents"
shipping$incidents <- factor(shipping$incidents)

#only "1" in incidents2
shipping$incidents2[shipping$incidents2==-99] <- NA
shipping$incidents2[is.na(shipping$incidents2)] <- 0
shipping$incidents2[shipping$incidents2==0] <- 1
shipping$Incident_action_recode <- NULL

#changing "Cocos Is." to "Australia"
shipping$closest_coastal_state[shipping$closest_coastal_state=="Cocos Is."] <- "Australia"

#changing "Paracel Islands" to "China" (no ISo2 code for Paracel Islands)
shipping$closest_coastal_state[shipping$closest_coastal_state=="Paracel Islands"] <- "China"
shipping$territorial_water_status[shipping$territorial_water_status=="Paracel Islands"] <- "China"

#changing "Spartly Islands" to "China" (no ISO2 code for Spratly Islands)
shipping$closest_coastal_state[shipping$closest_coastal_state=="Spratly Islands"] <- "China"
shipping$territorial_water_status[shipping$territorial_water_status=="Spratly Islands"] <- "China"

#changing "The Congo" to "Congo", otherwise duplication
shipping$closest_coastal_state[shipping$closest_coastal_state=="The Congo"] <- "Congo"

#changing "Ivory" to "Cote d'Ivoire", otherwise duplications
shipping$closest_coastal_state[shipping$closest_coastal_state=="Ivory "] <- "Cote d'Ivoire"

#Aggregate to Country Level
aggrtship <- dcast(shipping, closest_coastal_state + year ~ incidents, sum) #p317 R for Dummies

aggrtcc <- aggrtship$closest_coastal_state
aggrtship$iso2c <- countrycode(aggrtcc, "country.name", "iso2c")
aggrtship$closest_coastal_state <- NULL
aggrtship <- aggrtship[complete.cases(aggrtship),]

#African countries w/ sealine (World Bank)
aggrtship$continent <- "ROW"
aggrtship$continent[which(aggrtship$iso2c=="SD")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="ER")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="SO")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="KE")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="MZ")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="MG")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="ZA")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="NA")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="AO")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="CD")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="CG")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="GA")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="CM")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="GQ")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="NG")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="BJ")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="TG")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="GH")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="CI")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="LR")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="SL")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="GN")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="GW")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="GM")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="SN")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="CV")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="MR")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="TN")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="MU")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="SC")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="KM")] <- "Africa"
aggrtship$continent[which(aggrtship$iso2c=="TZ")] <- "Africa"

#East Asian Pacific countries w/ sealine (World Bank)
aggrtship$continent[which(aggrtship$iso2c=="JP")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="CN")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="KH")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="ID")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="KR")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="KP")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="MY")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="MM")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="PG")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="PH")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="SG")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="TH")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="TL")] <- "EastAsia"
aggrtship$continent[which(aggrtship$iso2c=="VN")] <- "EastAsia"

#South Asian Pacific countries w/ sealine (World Bank)
aggrtship$continent[which(aggrtship$iso2c=="BD")] <- "SouthAsia"
aggrtship$continent[which(aggrtship$iso2c=="IN")] <- "SouthAsia"
aggrtship$continent[which(aggrtship$iso2c=="MV")] <- "SouthAsia"
aggrtship$continent[which(aggrtship$iso2c=="PK")] <- "SouthAsia"
aggrtship$continent[which(aggrtship$iso2c=="LK")] <- "SouthAsia"

#MENA Region countries w/ sealine (World Bank)
aggrtship$continent[which(aggrtship$iso2c=="EG")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="DJ")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="DZ")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="LY")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="MA")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="BH")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="IR")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="IL")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="IQ")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="JO")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="KW")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="LB")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="LY")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="MA")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="OM")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="QA")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="SA")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="SY")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="TN")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="AE")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="PS")] <- "MENA"
aggrtship$continent[which(aggrtship$iso2c=="YE")] <- "MENA"

aggrtship$incidents2 <- aggrtship$incidents
aggrtship2 <- dcast(aggrtship, year ~ incidents, sum) #p317 R for Dummies
aggrtship2$ytotal <- rowSums(aggrtship2[,2:65])

Africa <- aggrtship[which(aggrtship$continent=="Africa"),]
summary(Africa$incidents)
table(Africa$incidents)
mean(Africa$incidents)
var(Africa$incidents)

MENA <- aggrtship[which(aggrtship$continent=="MENA"),]
table(MENA$incidents)
summary(MENA$incidents)
mean(MENA$incidents)
var(MENA$incidents)

SAsia <- aggrtship[which(aggrtship$continent=="SouthAsia"),]
table(SAsia$incidents)
summary(SAsia$incidents)
mean(SAsia$incidents)
var(SAsia$incidents)

EAsia <- aggrtship[which(aggrtship$continent=="EastAsia"),]
table(EAsia$incidents)
summary(EAsia$incidents)
mean(EAsia$incidents)
var(EAsia$incidents)

Asia <- aggrtship[which(aggrtship$continent=="EastAsia"|aggrtship$continent=="SouthAsia"),]
table(Asia$incidents)
summary(Asia$incidents)
mean(Asia$incidents)
var(Asia$incidents)

ROW <- aggrtship[which(aggrtship$continent=="ROW"),]
table(ROW$incidents)
summary(ROW$incidents)
mean(ROW$incidents)
var(ROW$incidents)

sum(aggrtship$incidents)
Afsum <- sum(Africa$incidents)
MEsum <- sum(MENA$incidents)
Assum <- sum(Asia$incidents)
ROWsum <- sum(ROW$incidents)
sum(Afsum + MEsum + Assum + ROWsum)

<<<<<<< HEAD
farben = c("firebrick1", "chocolate4", "mediumspringgreen", "deepskyblue2")
=======
farben = c("tomato", "cyan2", "darkolivegreen4", "darkorchid1")
>>>>>>> 0323bade769ea616e8eaa015aedcb9804d9e43e6

slices <- c(Afsum, MEsum, Assum, ROWsum) 
lbls <- c("Africa", "MENA", "Asia", "ROW")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=farben,
<<<<<<< HEAD
    main="Fig. 3 - Share of Incidents of Maritime Piracy 1993-2014")
=======
    main="Share of Incidents of Maritime Piracy among Regions 1993-2014")
>>>>>>> 0323bade769ea616e8eaa015aedcb9804d9e43e6

aggrtship$continent2 <- aggrtship$continent
aggrtship$incidents2 <- aggrtship$incidents
aggrtship$continent2 <- as.character(aggrtship$continent2)
aggrtship$continent2[which(aggrtship$continent=="EastAsia")] <- "Asia"
aggrtship$continent2[which(aggrtship$continent=="SouthAsia")] <- "Asia"
aggrtship$continent2 <- as.factor(aggrtship$continent2)
aggrtship <- aggrtship[,c(1,3,4,6,2,5)]
aggrtcon <- dcast(aggrtship, continent2 + year ~ incidents, sum) #p317 R for Dummies
aggrtcon$ytotal <- rowSums(aggrtcon[,3:66])

<<<<<<< HEAD
p <- ggplot(data = aggrtcon, aes(x = year, y = ytotal, group = continent2, color = continent2)) + geom_line() + ggtitle("Fig. 4 - Incidents of Maritime Piracy 1993-2014") + labs(x = "Year", y = "No. of Incidents")
p + scale_colour_discrete(name  ="Region", labels=c("Africa", "Asia", "MENA", "ROW"))

q <- ggplot(data = aggrtship2, aes(x = year, y = ytotal)) + geom_line()
q + ggtitle("Fig. 5 - Global Incidents of Maritime Piracy 1993-2014") + labs(x = "Year", y = "No. of Incidents")
=======
p <- ggplot(data = aggrtcon, aes(x = year, y = ytotal, group = continent2, color = continent2)) + geom_line() + ggtitle("Incidents of Maritime Piracy 1993-2014 per Region") + labs(x = "Year", y = "No. of Incidents")
p + scale_colour_discrete(name  ="Region", labels=c("Africa", "Asia", "MENA", "ROW"))

q <- ggplot(data = aggrtship2, aes(x = year, y = ytotal)) + geom_line()
q + ggtitle("Incidents of Maritime Piracy 1993-2014 World Total") + labs(x = "Year", y = "No. of Incidents")
>>>>>>> 0323bade769ea616e8eaa015aedcb9804d9e43e6


aggrtc <- dcast(aggrtship, iso2c ~ incidents, sum) #p317 R for Dummies
aggrtc$ctotal <- rowSums(aggrtc[,2:65])


aggrtc$category <- aggrtc$ctotal
<<<<<<< HEAD
aggrtc$category <- cut(aggrtc$ctotal, c(1,2,3,10,50,100,2000))
=======
aggrtc$category <- cut(aggrtc$ctotal, c(0,10,50,100,500,2000))
>>>>>>> 0323bade769ea616e8eaa015aedcb9804d9e43e6

d <- data.frame(country=c(aggrtc$iso2c),
                value=aggrtc$category)
n <- joinCountryData2Map(d,
                         joinCode="ISO2", 
                         nameJoinColumn="country",
                         mapResolution = "high")
<<<<<<< HEAD
#palegoldenrod=0, burlywood1=1, lightgreen=2-3, darkgreen=4-10, yellow=11-50, orange=51-100, red>100
mapCountryData(n, 
               nameColumnToPlot="value", 
               mapTitle="Fig. 5a - Incidents per Country 1993-2014",
               mapRegion="Africa",
               catMethod="categorical",
               colourPalette=c('burlywood1', 'lightgreen', 'darkgreen', 'yellow', 'orange', 'red'),
               addLegend=FALSE,
               borderCol="black",
               missingCountryCol="palegoldenrod",
=======
#burlywood1', 'lightgreen', 'darkgreen', 'yellow', 'orange', 'red'
#palegoldenrod=0, burlywood1=1, lightgreen=2-3, darkgreen=4-10, yellow=11-50, orange=51-100, red>100
mapCountryData(n, 
               nameColumnToPlot="value", 
               mapTitle="Incidents per Country 1993-2014 - Asia",
               mapRegion="Asia",
               catMethod="categorical",
               colourPalette=c('greenyellow', 'yellow', 'orange', 'red', 'darkred'),
               addLegend=TRUE,
               borderCol="black",
               missingCountryCol="lightgrey",
>>>>>>> 0323bade769ea616e8eaa015aedcb9804d9e43e6
               oceanCol = "lightblue")
mapCountryData(n, 
               nameColumnToPlot="value",
               mapTitle="Fig. 5b - Incidents per Country 1993-2014",
               mapRegion="Asia",
               catMethod="categorical",
               colourPalette=c('burlywood1', 'lightgreen', 'darkgreen', 'yellow', 'orange', 'red'),
               addLegend=FALSE,
               borderCol="black",
               missingCountryCol="palegoldenrod",
               oceanCol = "lightblue")
mapCountryData(n, 
               nameColumnToPlot="value", 
               mapTitle="Fig. 5c - Incidents per Country 1993-2014",
               mapRegion="Latin America",
               catMethod="categorical",
               colourPalette=c('burlywood1', 'lightgreen', 'darkgreen', 'yellow', 'orange', 'red'),
               addLegend=FALSE,
               borderCol="black",
               missingCountryCol="palegoldenrod",
               oceanCol = "lightblue")