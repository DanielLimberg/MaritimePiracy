##############################################
##############################################
# Merge Test 2 - Merge Test 2 - Merge Test 2 #
##############################################
##############################################
#Import the dataset about piracy incidentss into your wd 
#Call libraries we need for the project, make sure you have them installed
library(base)
library(rio) #swiss army knife for imports
library(plyr) #count occurences
library(dplyr) #data wrangling
library(tidyr) #data wrangling
library(ggplot2) #nice plots
library(stargazer) #nicer regression output which looks like a real publication
library(car) #scatterplots 
library(httr) #scraping from http sites
library(XML) #tool for generating XML file
library(WDI) #scraping Data from the World Bank
library(countrycode) #provides world bank country codes 
library(Amelia) #eyeballing missing values
library(tidyr) #reshaping
library(plm) #fixed and random effects models
library(stringr)
library(sjPlot) #makes nice graphs, see: http://strengejacke.de/sjPlot/ 

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracyWrangling/Data"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub/MaritimePiracyWrangling/Data"),silent=TRUE)
getwd()



####################################
####################################
# Scraping Data from World Bank - BB
####################################
####################################
#get rid of NA for WDI parsing with factor vectors
sea0 <- read.csv("sealine.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE, na.strings = c("", "NA"))

#deleting row 172 containing United States Pacific Island Wildlife Refuges whose ISO2 code is also US (and NAs)
#country names in ISO2
cc <- unique(na.omit(sea0)$country)
sea0$iso2c <- countrycode(cc, "country.name", "iso2c")
cc <- sea0$iso2c[!duplicated(sea0$iso2c)]
iso <- sea0$iso2c[!duplicated(sea0$iso2c)]

#parsing desired data from World Bank
allWDI <- WDI(iso, indicator = c("SL.UEM.TOTL.ZS", #unem.total (4th column)
                                 "SL.UEM.1524.MA.ZS", #unem.youth.m
                                 "SL.UEM.1524.ZS", #unem.youth
                                 "NY.GDP.PCAP.KD.ZG", #GDPpc
                                 "SP.POP.GROW", #pop.grow
                                 "SP.RUR.TOTL.ZG", #pop.rur.grow
                                 "SP.URB.GROW", #pop.urb.grow
                                 "FP.CPI.TOTL.ZG", #infl
                                 "SL.TLF.ACTI.1524.ZS", #labor.part
                                 "IT.CEL.SETS", #mobile
                                 "IT.CEL.SETS.P2", #mobile.p100
                                 "NY.GDP.MKTP.KD.ZG", #GDP.grow
                                 "SL.AGR.EMPL.ZS", #agrar.empl
                                 "SI.POV.GINI", #GINI index
                                 "SH.DYN.MORT", #Child mortality under 5 yrs (18th column)
                                 "BAR.POP.1519.FE", #female1519
                                 "BAR.POP.1519", #total1519
                                 "BAR.POP.2024.FE", #female2024
                                 "BAR.POP.2024", #total2024
                                 "BAR.POP.2529.FE", #female2529
                                 "BAR.POP.2529", #total2529
                                 "BAR.POP.3034.FE", #female3034
                                 "BAR.POP.3034"), #total3034
              start=1993, end=2014)

names(allWDI)[4] <- 'unem.total'
names(allWDI)[5] <- 'unem.youth.m'
names(allWDI)[6] <- 'unem.youth'
names(allWDI)[7] <- 'GDPpc'
names(allWDI)[8] <- 'pop.grow'
names(allWDI)[9] <- 'pop.rur.grow'
names(allWDI)[10] <- 'pop.urb.grow'
names(allWDI)[11] <- 'infl'
names(allWDI)[12] <- 'labor.part'
names(allWDI)[13] <- 'mobile'
names(allWDI)[14] <- 'mobile.p100'
names(allWDI)[15] <- 'GDP.grow'
names(allWDI)[16] <- 'agrar.empl'
names(allWDI)[17] <- 'gini.index'
names(allWDI)[18] <- 'child.mortality'
names(allWDI)[19] <- 'female1519'
names(allWDI)[20] <- 'total1519'
names(allWDI)[21] <- 'female2024'
names(allWDI)[22] <- 'total2024' 
names(allWDI)[23] <- 'female2529'
names(allWDI)[24] <- 'total2529'
names(allWDI)[25] <- 'female3034'
names(allWDI)[26] <- 'total3034'
allWDI[683:702, ] #Cape Verde
allWDI <- allWDI[-c(683:702), ] #delete Cape Verde and only keep Cabo Verde

allWDI$continent <- allWDI$iso2c
allWDI$continent[allWDI$continent=="EG"] <- "Africa"
allWDI$continent[allWDI$continent=="SD"] <- "Africa"
allWDI$continent[allWDI$continent=="ER"] <- "Africa"
allWDI$continent[allWDI$continent=="DJ"] <- "Africa"
allWDI$continent[allWDI$continent=="SO"] <- "Africa"
allWDI$continent[allWDI$continent=="KE"] <- "Africa"
allWDI$continent[allWDI$continent=="MZ"] <- "Africa"
allWDI$continent[allWDI$continent=="MG"] <- "Africa"
allWDI$continent[allWDI$continent=="ZA"] <- "Africa"
allWDI$continent[allWDI$continent=="NA"] <- "Africa"
allWDI$continent[allWDI$continent=="AO"] <- "Africa"
allWDI$continent[allWDI$continent=="CD"] <- "Africa"
allWDI$continent[allWDI$continent=="CG"] <- "Africa"
allWDI$continent[allWDI$continent=="GA"] <- "Africa"
allWDI$continent[allWDI$continent=="CM"] <- "Africa"
allWDI$continent[allWDI$continent=="GQ"] <- "Africa"
allWDI$continent[allWDI$continent=="NG"] <- "Africa"
allWDI$continent[allWDI$continent=="BJ"] <- "Africa"
allWDI$continent[allWDI$continent=="TG"] <- "Africa"
allWDI$continent[allWDI$continent=="GH"] <- "Africa"
allWDI$continent[allWDI$continent=="CI"] <- "Africa"
allWDI$continent[allWDI$continent=="LR"] <- "Africa"
allWDI$continent[allWDI$continent=="SL"] <- "Africa"
allWDI$continent[allWDI$continent=="GN"] <- "Africa"
allWDI$continent[allWDI$continent=="GW"] <- "Africa"
allWDI$continent[allWDI$continent=="GM"] <- "Africa"
allWDI$continent[allWDI$continent=="SN"] <- "Africa"
allWDI$continent[allWDI$continent=="CV"] <- "Africa"
allWDI$continent[allWDI$continent=="MR"] <- "Africa"
allWDI$continent[allWDI$continent=="MA"] <- "Africa"
allWDI$continent[allWDI$continent=="DZ"] <- "Africa"
allWDI$continent[allWDI$continent=="LY"] <- "Africa"
allWDI$continent[allWDI$continent=="TN"] <- "Africa"
allWDI$continent[allWDI$continent=="MU"] <- "Africa"
allWDI$continent[allWDI$continent=="SC"] <- "Africa"
allWDI$continent[allWDI$continent=="KM"] <- "Africa"



##############################
##############################
# Merge the data sets - Lim.D.
##############################
##############################

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
shipping$vessel[shipping$vessel==111] <- 1
shipping$vessel[shipping$vessel==222] <- 2
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
shipping$incidents <- factor(shipping$incidents,
                             levels = c(0,1),
                             labels = c("incidents", "incidents"))
shipping$incidents <- factor(shipping$incidents)

#only "1" in incidents2
shipping$incidents2[shipping$incidents2==-99] <- NA
shipping$incidents2[is.na(shipping$incidents2)] <- 0
shipping$incidents2[shipping$incidents2==0] <- 1
shipping$Incident_action_recode <- NULL

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
library(reshape2)
aggrtship <- dcast(shipping, closest_coastal_state + year ~ incidents, sum) #p317 R for Dummies

aggrtcc <- aggrtship$closest_coastal_state
aggrtship$iso2c <- countrycode(aggrtcc, "country.name", "iso2c")
aggrtship$closest_coastal_state <- NULL

#aggrtship[!(duplicated(aggrtship[c("iso2c","year")]) | duplicated(aggrtship[c("iso2c","year")], fromLast = TRUE)), ]
#aggrtship[duplicated(aggrtship[,1:2]),]
#unique(is.na(aggrtship$iso2c))



###################################
# MERGE 1 ### MERGE 1 ### MERGE 1 #
###################################
merge1 <- merge(allWDI,aggrtship,by=c("iso2c", "year"), all.x = TRUE) #merges WDI + data on piracy incidentss per country --> yes, works for us
#TW, GF, MQ, TF, and IO are not in merge1, these incidents are omitted
#merge1[!(duplicated(merge1[c("iso2c","year")]) | duplicated(merge1[c("iso2c","year")], fromLast = TRUE)), ]
#merge1[duplicated(merge1[,1:2]),]
#anti_join(allWDI,aggrtship,by= "iso2c", "year")
rm(allWDI, aggrtship, shipping, aggrtcc, cc, iso, sea0)
missmap(merge1) #eyeballing missing data



####################
#Armed Conflict data
####################
military2 <- read.csv("non-state-conflict.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
mil2cc <- military2$location #155 unique values
military2$iso2c <- countrycode(mil2cc, "country.name", "iso2c") #only 84 iso countries
mil2 <- ddply(military2, .(iso2c, year), function(military2) c(bestfatalityestimate=sum(military2$bestfatalityestimate), lowfatalityestimate=sum(military2$lowfatalityestimate), highfatalityestimate=sum(military2$highfatalityestimate)))
aggrtmil2 <- data.frame(mil2)



###################################
# MERGE 2 ### MERGE 2 ### MERGE 2 #
###################################
merge2 <- merge(merge1,aggrtmil2,by=c("iso2c", "year"), all.x = TRUE) #merges merge2 + aggrtmil2
rm(aggrtmil2, mil2, military2, mil2cc)
merge2$incidents[is.na(merge2$incidents)] <- 0
merge2$bestfatalityestimate[is.na(merge2$bestfatalityestimate)] <- 0
merge2$lowfatalityestimate[is.na(merge2$lowfatalityestimate)] <- 0
merge2$highfatalityestimate[is.na(merge2$highfatalityestimate)] <- 0

missmap(merge2) #eyeballing missing data
#merge2[!(duplicated(merge2[c("iso2c","year")]) | duplicated(merge2[c("iso2c","year")], fromLast = TRUE)), ]
#merge2[duplicated(merge2[,1:2]),]
#summary(merge2$incidents) #Min: 0, Max: 136



#############################################
#Eyeballing the dependent variable: incidents
#############################################
hist(merge2$incidents, main="Incidents of Piracy", col="blue", breaks = 100) #poisson distribution, zero inflated
summary(merge2$incidents)
temp <- filter(merge2, incidents != 0) #only 804 observations left if all 0 deleted
hist(temp$incidents, main="Incidents of Piracy", col="green", breaks = 100)
table(temp$incidents)
temp <- filter(merge2, incidents != 0, incidents != 32, incidents != 33, incidents != 34, incidents != 35, incidents != 36, incidents != 37, incidents != 39, incidents != 40, incidents != 45, incidents != 47, incidents != 50, incidents != 51, incidents != 54, incidents != 55, incidents != 58, incidents != 60, incidents != 62, incidents != 68, incidents != 71, incidents != 74, incidents != 76, incidents != 82, incidents != 84, incidents != 85, incidents != 87, incidents != 99, incidents != 100, incidents != 110, incidents != 111, incidents != 114, incidents != 117, incidents != 130, incidents != 131, incidents != 136)
hist(temp$incidents, main="Incidents of Piracy", col="orange", ylim=c(1,400), xlim=c(0,30))
table(temp$incidents)
rm(temp)



##############
#Disaster Data
##############
disaster <- read.csv("disaster_total.csv", header = TRUE, sep = ",", ".", stringsAsFactors = FALSE, na.strings = c("", "NA"))
names(disaster)[3] <- 'country' #former var name: Country.name
names(disaster)[4] <- 'disaster' #former var name: X.disaster.type.

disaster$X.Country.iso <- NULL
disaster$X.Total.deaths. <- NULL
disaster$X.Total.affected. <- NULL
disaster$X.Total.damage. <- NULL
disaster$Affected <- NULL
disaster$Injured <- NULL
disaster$Homeless <- NULL

disaster$disaster <- gsub("[^a-zA-Z0-9]","",disaster$disaster) #get rid of special characters
disaster$country <- gsub("[^a-zA-Z0-9]","",disaster$country) #get rid of special characters
list(disaster$country)

aggrtdis <- dcast(disaster, country + year ~ disaster, sum) #p317 R for Dummies
disastercc <- aggrtdis$country
aggrtdis$iso2c <- countrycode(disastercc, "country.name", "iso2c")

aggrtdis$Animalaccident <- NULL
aggrtdis$Extremetemperature <- NULL
aggrtdis$Insectinfestation <- NULL
aggrtdis$Massmovementdry <- NULL
aggrtdis$Volcanicactivity <- NULL
aggrtdis$country <- NULL

aggrtdis[1740, ] #NetherlandsAntilles
aggrtdis <- aggrtdis[-c(1740), ] #delete "NetherlandsAntilles" which is assigned ISO2 code NL
aggrtdiscomplete <- aggrtdis[complete.cases(aggrtdis),] #delete country "CanaryIs" which has no ISO2 code (NA)

#Dought dummy: DD
aggrtdiscomplete$DD <- aggrtdiscomplete$Drought
aggrtdiscomplete$DD[aggrtdiscomplete$DD==2] <- 1
aggrtdiscomplete$DD[aggrtdiscomplete$DD==3] <- 1

#Flood dummy: FD
aggrtdiscomplete$FD <- aggrtdiscomplete$Flood
aggrtdiscomplete$FD[aggrtdiscomplete$FD==2] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==3] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==4] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==5] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==6] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==7] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==8] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==9] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==10] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==11] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==12] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==13] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==14] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==15] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==16] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==17] <- 1
aggrtdiscomplete$FD[aggrtdiscomplete$FD==20] <- 1



###################################
# MERGE 3 ### MERGE 3 ### MERGE 3 #
###################################
merge3 <- merge(merge2,aggrtdiscomplete,by=c("iso2c", "year"), all.x = TRUE) #merges conflict2 + aggrtdis
rm(aggrtdis, disaster, disastercc, aggrtdiscomplete)
merge3$Earthquake <- NULL
merge3$Epidemic <- NULL
merge3$Impact <- NULL
merge3$Landslide <- NULL
merge3$Storm <- NULL
merge3$Wildfire <- NULL
merge3$Drought[is.na(merge3$Drought)] <- 0
merge3$Flood[is.na(merge3$Flood)] <- 0
merge3$DD[is.na(merge3$DD)] <- 0
merge3$FD[is.na(merge3$FD)] <- 0
missmap(merge3) #eyeballing missing data
summary(merge3$gini.index) #2881 NA's, only 813 values
temp <- filter(merge3, incidents != 0) #only 804 observations left if all 0 deleted
summary(temp$gini.index) #592 NA's, only 212 values
rm(temp)



###################################
# MERGE 4 ### MERGE 4 ### MERGE 4 #
###################################
oil <- read.csv("oil_wti.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = c("", "NA"))
names(oil)[1] <- 'year'
str(oil$year)
oil$year <- as.character(oil$year)
oil$year <- substring(oil$year,1,nchar(oil$year)-6)
merge4 <- merge(merge3,oil,by=c("year"), all.x = TRUE) #merges merge3 + oil price (WTI, annual average)
#Africa <- merge4[merge4$continent == "Africa", ]
#missmap(Africa)
rm(oil)



###############################################
###############################################
# Analysis Socio-Economic Determinants - Lim.D.
###############################################
###############################################
#merge3 <- read.csv("merge3.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
#Africa <- read.csv("africa.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
panel <- pdata.frame(merge4, index=c("iso2c", "year")) #setting dataframe to panel data
rm(merge1, merge2, merge3, merge4)



############################################
#Plot some explenatory var against incidents
############################################

#################
#GDPpc and piracy
#################
GDP = panel$GDPpc #x-axis
incidents = panel$incidents #y-axis
plot(GDP, incidents, xlab="GDP per capita", ylab="Incidents of Piracy")

######################
#GDP growth and piracy
######################
GDP = panel$GDP.grow #x-axis
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
GDP = panel$GDP.grow #y-axis
plot(mobile, GDP, xlab="Mobile Phones", ylab="GDP growth")

################################
#Mobile Phones per 100 and GDPpc
################################
mobile = panel$mobile.p100 #x-axis
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
mobile = panel$mobile.p100 #x-axis
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
gini = panel$gini.index #x-axis
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
GDP = temp$GDP.grow #x-axis
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
GDP = temp$GDP.grow #y-axis
plot(mobile, GDP, xlab="Mobile Phones", ylab="GDP growth")

################################
#Mobile Phones per 100 and GDPpc
################################
mobile = temp$mobile.p100 #x-axis
GDP = temp$GDPpc #y-axis
plot(mobile, incidents, xlab="Mobile Phones per 100", ylab="GDP per capita")

#####################################
#Mobile phones per 100 and GDP growth
#####################################
mobile = temp$mobile.p100 #x-axis
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
mobile = temp$mobile.p100 #x-axis
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
gini = temp$gini.index #x-axis
incidents = temp$incidents #y-axis
plot(gini, incidents, xlab="GINI index", ylab="Incidents of Piracy")

rm(drought, flood, GDP, incidents, mobile, temp, gini)

#Hausman test: comparing fixed and random effects model
#fe <- plm(incidents~mobile+unem.total+lowfatalityestimate, data=panel, index=c("iso2c", "year"), model="within") #fixed effects model
#re <- plm(incidents~mobile+unem.total+lowfatalityestimate, data=panel, index=c("iso2c", "year"), model="random") #random effects model
#ols <- plm(incidents~mobile+unem.total+lowfatalityestimate, data=panel, index=c("iso2c", "year"), model="pooling") #random effects model
#summary(fe)
#summary(re)
#summary(ols)
#vif(fe)
#vif(re)
#vif(ols)
#phtest(fe, re) #Hausman, H0: cov(a,x)=0 #error: Error in solve.default(dvcov) : system is computationally singular: reciprocal condition number = 1.62162e-16
