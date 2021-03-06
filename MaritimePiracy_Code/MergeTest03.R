##############################################
##############################################
# Merge Test 3 - Merge Test 3 - Merge Test 3 #
##############################################
##############################################
#Import the dataset about piracy incidentss into your wd 
#Call libraries we need for the project, make sure you have them installed
library(base)
library(rio) #swiss army knife for imports
library(plyr) #command ddply
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
library(foreign)
library(MASS)
library(aod)
library(Rcpp)

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracyWrangling/Data"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
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
shipping$vessel <- factor(shipping$vessel)

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

#African countries w/ sealine (World Bank)
allWDI$continent <- "ROW"
allWDI$continent[which(allWDI$iso2c=="EG")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="SD")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="ER")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="DJ")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="SO")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="KE")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="MZ")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="MG")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="ZA")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="NA")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="AO")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="CD")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="CG")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="GA")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="CM")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="GQ")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="NG")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="BJ")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="TG")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="GH")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="CI")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="LR")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="SL")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="GN")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="GW")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="GM")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="SN")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="CV")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="MR")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="MA")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="DZ")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="LY")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="TN")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="MU")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="SC")] <- "Africa"
allWDI$continent[which(allWDI$iso2c=="KM")] <- "Africa"

#East Asian countries w/ sealine (World Bank)
allWDI$continent[which(allWDI$iso2c=="JP")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="CN")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="KH")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="ID")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="KR")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="MY")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="MM")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="PG")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="PH")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="SG")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="TH")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="TL")] <- "Asia"
allWDI$continent[which(allWDI$iso2c=="VN")] <- "Asia"

#East Asia country dummies
allWDI$Japan <- 0
allWDI$Japan[which(allWDI$iso2c=="JP")] <- 1
allWDI$China <- 0
allWDI$China[which(allWDI$iso2c=="CN")] <- 1
allWDI$Cambodia <- 0
allWDI$Cambodia[which(allWDI$iso2c=="KH")] <- 1
allWDI$Indonesia <- 0
allWDI$Indonesia[which(allWDI$iso2c=="ID")] <- 1
allWDI$Korea <- 0
allWDI$Korea[which(allWDI$iso2c=="KR")] <- 1
allWDI$Malaysia <- 0
allWDI$Malaysia[which(allWDI$iso2c=="MY")] <- 1
allWDI$Myanmar <- 0
allWDI$Myanmar[which(allWDI$iso2c=="MM")] <- 1
allWDI$Papua <- 0
allWDI$Papua[which(allWDI$iso2c=="PG")] <- 1
allWDI$Philippines <- 0
allWDI$Philippines[which(allWDI$iso2c=="PH")] <- 1
allWDI$Singapore <- 0
allWDI$Singapore[which(allWDI$iso2c=="SG")] <- 1
allWDI$Thailand <- 0
allWDI$Thailand[which(allWDI$iso2c=="TH")] <- 1
allWDI$Timor <- 0
allWDI$Timor[which(allWDI$iso2c == "TL")] <- 1

###################################
# MERGE 1 ### MERGE 1 ### MERGE 1 #
###################################
merge1 <- merge(allWDI,aggship,by=c("iso2c", "year"), all.x = TRUE) #merges WDI + data on piracy incidentss per country --> yes, works for us
#merge1[duplicated(merge1[,1:2]),]
#anti_join(allWDI,aggrtship,by= "iso2c", "year")
rm(aggship, allWDI, shipping, aggcc, iso)
merge1$incidents[is.na(merge1$incidents)] <- 0
missmap(merge1) #eyeballing missing data
hist(merge1$incidents, main="Incidents of Piracy", col="blue", breaks = 100) #poisson distribution
summary(merge1$incidents) #mean = 2.978
var(merge1$incidents) #variance = 113.9323
#African countries only
Africa <- filter(merge1, continent == "Africa")
hist(Africa$incidents, main="Incidents of Piracy", col="red", breaks = 100) #poisson distribution
summary(Africa$incidents) #mean = 1.865
var(Africa$incidents) #variance = 23.65033
#Asian countries only
Asia <- filter(merge1, continent == "Asia")
hist(Asia$incidents, main="Incidents of Piracy", col="yellow", breaks = 100) #poisson distribution
summary(Asia$incidents) #mean = 10.09
var(Asia$incidents) #variance = 548.2198
rm(Africa, Asia)

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
#temp <- filter(merge3, incidents != 0)
#summary(temp$gini.index)
#rm(temp)

###################################
# MERGE 4 ### MERGE 4 ### MERGE 4 #
###################################
oil <- read.csv("oil_wti.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = c("", "NA"))
names(oil)[1] <- 'year' #former var name: DATE
str(oil$year)
oil$year <- as.character(oil$year)
oil$year <- substring(oil$year,1,nchar(oil$year)-6)
merge4 <- merge(merge3,oil,by=c("year"), all.x = TRUE) #merges merge3 + oil price (WTI, annual average)
#Africa <- merge4[merge4$continent == "Africa", ]
#missmap(Africa)
rm(oil)