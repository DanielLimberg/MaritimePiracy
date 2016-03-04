##############################################
##############################################
# Merge Test 2 - Merge Test 2 - Merge Test 2 #
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
library(tidyr) #reshaping
library(plm) #fixed and random effects models
library(stringr)
library(sjPlot) #makes nice graphs, see: http://strengejacke.de/sjPlot/ 
library(foreign)
library(MASS)
library(aod)
library(Rcpp)
library(XML)
library(RCurl)

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracyWrangling/Data"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
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
                                 "SL.UEM.1524.ZS", #unem.y
                                 "SL.UEM.1524.MA.ZS", #unem.y.m
                                 "NY.GDP.MKTP.KD", #GDP, constant 2005 USD
                                 "NY.GDP.MKTP.KD.ZG", #GDP.gr, constant 2005 USD
                                 "NY.GDP.PCAP.KD.ZG", #GDPpc.gr, constant 2005 USD
                                 "IT.CEL.SETS", #mobile
                                 "IT.CEL.SETS.P2", #mobile per 100 people
                                 "SP.POP.GROW", #pop.gr
                                 "SP.RUR.TOTL.ZG", #poprur.gr
                                 "SP.URB.GROW", #popurb.gr
                                 "SH.DYN.MORT", #child mortatlity <5 yrs per 1000
                                 "IQ.CPA.PROP.XQ", #property rights and rule-based governance rating
                                 "SL.AGR.EMPL.ZS", #empl.agrar
                                 "SI.POV.GINI", #gini
                                 "NY.GNP.PCAP.KD"), #GNIpc, constant 2005 USD
              start=1993, end=2014)

missmap(allWDI) #eyeballing missing data

names(allWDI)[4] <- 'unem.total'
names(allWDI)[5] <- 'unem.y'
names(allWDI)[6] <- 'unem.y.m'
names(allWDI)[7] <- 'GDP'
names(allWDI)[8] <- 'GDP.gr'
names(allWDI)[9] <- 'GDPpc.gr'
names(allWDI)[10] <- 'mobile'
names(allWDI)[11] <- 'mobilep100'
names(allWDI)[12] <- 'pop.gr'
names(allWDI)[13] <- 'poprur.gr'
names(allWDI)[14] <- 'popurb.gr'
names(allWDI)[15] <- 'cmort'
names(allWDI)[16] <- 'property'
names(allWDI)[17] <- 'empl.agrar'
names(allWDI)[18] <- 'gini'
names(allWDI)[19] <- 'GNIpc'

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

#East Asian Pacific countries w/ sealine (World Bank)
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

#theory suggests that GNIpc < 2000 a year might be more prone to piracy (Murphey2008)
allWDI$GNIgroup <- cut(allWDI$GNIpc, c(0,2000,4000,6000,10000,20000))

#country dummies
idx <- sort(unique(allWDI$iso2c))
dummy <- matrix(NA, nrow = nrow(allWDI), ncol = length(idx))
for (j in 1:length(idx)) { 
  dummy[,j] <- as.integer(allWDI$iso2c == idx[j])
}
rm(j)
#names(dummy) <- idx
#allWDI <- cbind(allWDI, dummy)

#z.out <- zelig(y ~ x1 + x2 + x3 + as.factor(iso2c), 
               #data = mydata, model = "ls")



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
rm(allWDI, aggrtship, shipping, aggrtcc, cc, iso, sea0, idx, dummy)
merge1$incidents[is.na(merge1$incidents)] <- 0
merge1$incbinary <- merge1$incidents
merge1$incbinary[merge1$incbinary>=1] <- 1
missmap(merge1) #eyeballing missing data
hist(merge1$incidents, main="Incidents of Piracy", col="blue", breaks = 100) #poisson distribution
summary(merge1$incidents) #mean = 1.725
var(merge1$incidents) #variance = 68.03806
#African countries only
Africa <- filter(merge1, continent == "Africa")
hist(Africa$incidents, main="Incidents of Piracy", col="red", breaks = 100) #poisson distribution
summary(Africa$incidents) #mean = 1.674
var(Africa$incidents) #variance = 21.52207
#Asian countries only
Asia <- filter(merge1, continent == "Asia")
hist(Asia$incidents, main="Incidents of Piracy", col="yellow", breaks = 100) #poisson distribution
summary(Asia$incidents) #mean = 9.315
var(Asia$incidents) #variance = 513.1568
rm(Africa, Asia)


####################
#Armed Conflict data
####################
conflict <- read.csv("non-state-conflict.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
#renaming, otherwise no ISO2 code
conflict$location[conflict$location=="Yemen (North Yemen)"] <- "Yemen"
conflcc <- conflict$location
conflict$startdate2 <- as.character(conflict$startdate2)
conflict$startdate2 <- substring(conflict$startdate2,1,nchar(conflict$startdate2)-6)
names(conflict)[16] <- 'year2' #former var name: year
names(conflict)[11] <- 'year' #former var name: startdate2
conflict$iso2c <- countrycode(conflcc, "country.name", "iso2c")
con <- ddply(conflict, .(iso2c, year), function(conflict) c(bestfatalityestimate=sum(conflict$bestfatalityestimate), lowfatalityestimate=sum(conflict$lowfatalityestimate), highfatalityestimate=sum(conflict$highfatalityestimate)))
aggcon <- data.frame(con)



###################################
# MERGE 2 ### MERGE 2 ### MERGE 2 #
###################################
merge2 <- merge(merge1,aggcon,by=c("iso2c", "year"), all.x = TRUE) #merges merge2 + aggrtmil2
rm(aggcon, con, conflict, conflcc)
merge2$bestfatalityestimate[is.na(merge2$bestfatalityestimate)] <- 0
merge2$lowfatalityestimate[is.na(merge2$lowfatalityestimate)] <- 0
merge2$highfatalityestimate[is.na(merge2$highfatalityestimate)] <- 0
#create categorical variable
summary(merge2$lowfatalityestimate)
merge2$battlelow <- cut(merge2$lowfatalityestimate, c(-1,25,500,10000))
table(merge2$battlelow)
missmap(merge2) #eyeballing missing data



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


###################################
# MERGE 5 ### MERGE 5 ### MERGE 5 #
###################################
URL <- "https://www.cia.gov/library/publications/the-world-factbook/fields/2060.html" #length of coastline
page <- getURL(URL, .opts = list(ssl.verifypeer=FALSE))
tables <- readHTMLTable(page, header=TRUE)

length <- data.frame(tables)
names(length)[1] <- 'country'
names(length)[2] <- 'coastkm'
length$coastkm <- as.character(length$coastkm)
length$coastkm <- gsub("[,total: ]","",length$coastkm)
length$coastkm <- gsub("\\k.*","",length$coastkm)
length$coastkm <- as.numeric(length$coastkm)

lcc <- length$country
length$iso2c <- countrycode(lcc, "country.name", "iso2c")
length[261, ] #West Bank, same iso2 code as Gaza Strip
length <- length[-c(261), ]
length <- na.omit(length)
length$country <- NULL
legnth$coastkm <- as.numeric(lenght$coastkm)
merge5 <- merge(merge4,length,by=c("iso2c"), all.x = TRUE) #merges merge4 + coastline length
rm(length, lcc)



###################################
# MERGE 6 ### MERGE 6 ### MERGE 6 #
###################################
URL <- "https://www.cia.gov/library/publications/the-world-factbook/rankorder/2147rank.html" #square kilometers
page <- getURL(URL, .opts = list(ssl.verifypeer=FALSE))
tables <- readHTMLTable(page, header=TRUE)

area <- data.frame(tables)
area <- area[,c(2:3)]
names(area)[1] <- 'country'
names(area)[2] <- 'sqkm'
area$sqkm <- as.character(area$sqkm)
area$sqkm <- gsub("[, ]","",area$sqkm)
#acc <- area$country
#area$iso2c <- countrycode(acc, "country.name", "iso2c")
area[172, ] #West Bank, same iso2 code as Gaza Strip
area <- area[-c(172), ]
area[239, ] #US Pacific Island Wildlife Refuges, same iso2 code as USA
area <- area[-c(239), ]
#area$country <- NULL
area$sqkm <- as.numeric(area$sqkm)
merge6 <- merge(merge5,area,by=c("country"), all.x = TRUE) #merges merge4 + coastline length
rm(area, acc)



###################################
# MERGE 7 ### MERGE 7 ### MERGE 7 #
###################################
URL <- "https://www.cia.gov/library/publications/the-world-factbook/fields/2096.html" #lenght of border
page <- getURL(URL, .opts = list(ssl.verifypeer=FALSE))
tables <- readHTMLTable(page, header=TRUE)

border <- data.frame(tables)
names(border)[1] <- 'country'
names(border)[2] <- 'borderkm'
bcc <- border$country
border$iso2c <- countrycode(bcc, "country.name", "iso2c")
border[247, ] #US Pacific Island Wildlife Refuges, same iso2 code as USA
border[257, ] #West Bank, same iso2 code as Gaza Strip
border <- border[-c(247,257), ]
border$borderkm <- as.character(border$borderkm)
border$borderkm <- gsub("[,total: ]","",border$borderkm)
border$borderkm <- gsub("[merpinFrnce-]","",border$borderkm)
border$borderkm <- gsub("\\k.*","",border$borderkm)
border$borderkm <- as.numeric(border$borderkm)
border$country <- NULL
merge7 <- merge(merge6,border,by=c("iso2c"), all.x = TRUE) #merges merge5 + border length
rm(border, bcc, tables, page, URL)



###################################
# MERGE 8 ### MERGE 8 ### MERGE 8 #
###################################
polity <- read.csv("p4v2014.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA")) #polity iv project
polity <- polity[,c(4:5,8:9,11)]
#pcc <- polity$country
#polity$iso2c <- countrycode(pcc, "country.name", "iso2c")
#polity$country <- NULL
#polity$polity[polity$polity==-88] <- NA
#polity$polity[polity$polity==-77] <- NA
#polity$polity[polity$polity==-66] <- NA
#polity$polity2[polity$polity2==-10] <- 111
#polity$polity2[polity$polity2==-9] <- 111
#polity$polity2[polity$polity2==-8] <- 111
#polity$polity2[polity$polity2==-7] <- 111
#polity$polity2[polity$polity2==-6] <- 111
#polity$polity2[polity$polity2==-5] <- 222
#polity$polity2[polity$polity2==-4] <- 222
#polity$polity2[polity$polity2==-3] <- 222
#polity$polity2[polity$polity2==-2] <- 222
#polity$polity2[polity$polity2==-1] <- 222
#polity$polity2[polity$polity2==0] <- 222
#polity$polity2[polity$polity2==1] <- 222
#polity$polity2[polity$polity2==2] <- 222
#polity$polity2[polity$polity2==3] <- 222
#polity$polity2[polity$polity2==4] <- 222
#polity$polity2[polity$polity2==5] <- 222
#polity$polity2[polity$polity2==6] <- 111
#polity$polity2[polity$polity2==7] <- 111
#polity$polity2[polity$polity2==8] <- 111
#polity$polity2[polity$polity2==9] <- 111
#polity$polity2[polity$polity2==10] <- 111
#polity$polity2[polity$polity2==111] <- 1
#polity$polity2[polity$polity2==222] <- 2
#polity$polity2[polity$polity2==333] <- 3
#polity$polity2 <- factor(polity$polity2,
#                          levels = c(1,2,3),
#                          labels = c("aut-dem", "anocracy"))
polity <- polity[,c(1:2,5)]
merge8 <- merge(merge7,polity,by=c("country", "year"), all.x = TRUE) #merges merge5 + polity data series (why does it only work w/ 'country' not w/ 'iso2c'?)
missmap(merge8) #eyeballing missing data
rm(polity, pcc)



###############################################
###############################################
# Declare Panel Data
###############################################
###############################################
#merge3 <- read.csv("merge3.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
#Africa <- read.csv("africa.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
panel <- pdata.frame(merge8, index=c("iso2c", "year")) #setting dataframe to panel data
rm(merge1, merge2, merge3, merge4, merge5, merge6, merge7)
