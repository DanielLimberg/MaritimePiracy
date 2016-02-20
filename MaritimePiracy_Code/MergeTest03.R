##############################################
##############################################
# Merge Test 3 - Merge Test 3 - Merge Test 3 #
##############################################
##############################################
#Import the dataset about piracy incidentss into your wd 
#Call libraries we need for the project, make sure you have them installed
library(base)
library(rio) #swiss army knife for imports
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
library(plm) #fixed and random effects models
library(stringr)
library(sjPlot) #makes nice graphs, see: http://strengejacke.de/sjPlot/

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracyWrangling/Data"),silent=TRUE)
try(setwd("C:\Users\Dani\Documents\GitHub2\MaritimePiracy\MaritimePiracy_Data"),silent=TRUE)
getwd()

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

#changing "Paracel Islands" to "China" (no ISO2 code for Paracel Islands)
shipping$closest_coastal_state[shipping$closest_coastal_state=="Paracel Islands"] <- "China"
#changing "Spartly Islands" to "China" (no ISO2 code for Spratly Islands)
shipping$closest_coastal_state[shipping$closest_coastal_state=="Spratly Islands"] <- "China"
#changing "The Congo" to "Congo", otherwise duplication
shipping$closest_coastal_state[shipping$closest_coastal_state=="The Congo"] <- "Congo"
#changing "Ivory" to "Cote d'Ivoire", otherwise duplications
shipping$closest_coastal_state[shipping$closest_coastal_state=="Ivory "] <- "Cote d'Ivoire"
#changing "Democratic Republic of Sao Tome and Principe" to "Sao Tome & Principe"
shipping$closest_coastal_state[shipping$closest_coastal_state=="Democratic Republic of Sao Tome and Principe"] <- "Sao Tome & Principe"

#Aggregate to Country Level
library(reshape2)
aggship <- dcast(shipping, closest_coastal_state + year ~ incidents, sum) #p317 R for Dummies

aggcc <- aggship$closest_coastal_state
aggship$iso2c <- countrycode(aggcc, "country.name", "iso2c")
aggship$closest_coastal_state <- NULL

####################################
####################################
# Scraping Data from World Bank - BB
####################################
####################################
#country names in ISO2
iso <- aggship$iso2c[!duplicated(aggship$iso2c)]

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
                                 "SH.DYN.MORT"), #Child mortality under 5 yrs (18th column)
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

#African countries w/ sealine
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

#East Asian countries w/ sealine
allWDI$continent[allWDI$continent=="JP"] <- "Asia"
allWDI$continent[allWDI$continent=="CN"] <- "Asia"
allWDI$continent[allWDI$continent=="KA"] <- "Asia"
allWDI$continent[allWDI$continent=="ID"] <- "Asia"
allWDI$continent[allWDI$continent=="KR"] <- "Asia"
allWDI$continent[allWDI$continent=="MY"] <- "Asia"
allWDI$continent[allWDI$continent=="MM"] <- "Asia"
allWDI$continent[allWDI$continent=="PG"] <- "Asia"
allWDI$continent[allWDI$continent=="PH"] <- "Asia"
allWDI$continent[allWDI$continent=="SG"] <- "Asia"
allWDI$continent[allWDI$continent=="TH"] <- "Asia"
allWDI$continent[allWDI$continent=="TL"] <- "Asia"
allWDI$continent[allWDI$continent=="VN"] <- "Asia"

###################################
# MERGE 1 ### MERGE 1 ### MERGE 1 #
###################################
merge1 <- merge(allWDI,aggship,by=c("iso2c", "year"), all.x = TRUE) #merges WDI + data on piracy incidentss per country --> yes, works for us
#merge1[duplicated(merge1[,1:2]),]
#anti_join(allWDI,aggrtship,by= "iso2c", "year")
rm(aggship, allWDI, shipping, aggcc, iso)
missmap(merge1) #eyeballing missing data

####################
#Armed Conflict data
####################
conflict <- read.csv("non-state-conflict.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
#renaming, otherwise no ISO2 code
conflict$location[conflict$location=="Yemen (North Yemen)"] <- "Yemen"
conflcc <- conflict$location
conflict$startdate <- as.character(conflict$startdate)
conflict$startdate <- substring(conflict$startdate,1,nchar(conflict$startdate)-6)
names(conflict)[16] <- 'year2' #former var name: year
names(conflict)[9] <- 'year' #former var name: startdate
conflict$iso2c <- countrycode(conflcc, "country.name", "iso2c")
con <- ddply(conflict, .(iso2c, year), function(conflict) c(bestfatalityestimate=sum(conflict$bestfatalityestimate), lowfatalityestimate=sum(conflict$lowfatalityestimate), highfatalityestimate=sum(conflict$highfatalityestimate)))
aggcon <- data.frame(con)

###################################
# MERGE 2 ### MERGE 2 ### MERGE 2 #
###################################
merge2 <- merge(merge1,aggcon,by=c("iso2c", "year"), all.x = TRUE) #merges merge2 + aggcon
rm(aggcon, con, conflict, conflcc)
merge2$incidents[is.na(merge2$incidents)] <- 0
merge2$bestfatalityestimate[is.na(merge2$bestfatalityestimate)] <- 0
merge2$lowfatalityestimate[is.na(merge2$lowfatalityestimate)] <- 0
merge2$highfatalityestimate[is.na(merge2$highfatalityestimate)] <- 0
#create categorical variable
summary(merge2$lowfatalityestimate)
merge2$battlelow <- cut(merge2$lowfatalityestimate, c(-1,25,500,10000))
table(merge2$battlelow)
missmap(merge2) #eyeballing missing data

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
aggdiscomplete <- aggrtdis[complete.cases(aggrtdis),] #delete country "CanaryIs" which has no ISO2 code (NA)

#Dought dummy: DD
aggdiscomplete$DD <- aggdiscomplete$Drought
aggdiscomplete$DD[aggdiscomplete$DD==2] <- 1
aggdiscomplete$DD[aggdiscomplete$DD==3] <- 1

#Flood dummy: FD
aggdiscomplete$FD <- aggdiscomplete$Flood
aggdiscomplete$FD[aggdiscomplete$FD==2] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==3] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==4] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==5] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==6] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==7] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==8] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==9] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==10] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==11] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==12] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==13] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==14] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==15] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==16] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==17] <- 1
aggdiscomplete$FD[aggdiscomplete$FD==20] <- 1

###################################
# MERGE 3 ### MERGE 3 ### MERGE 3 #
###################################
merge3 <- merge(merge2,aggdiscomplete,by=c("iso2c", "year"), all.x = TRUE) #merges conflict2 + aggrtdis
rm(aggrtdis, disaster, disastercc, aggdiscomplete)
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
summary(merge3$gini.index)
temp <- filter(merge3, incidents != 0)
summary(temp$gini.index)
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