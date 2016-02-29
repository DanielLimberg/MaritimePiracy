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
                                 "SL.UEM.1524.ZS", #unem.youth
                                 "SL.UEM.1524.MA.ZS", #unem.youth.m
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
                                 "SI.POV.GINI"), #gini (18th column)
              start=1993, end=2014)

missmap(allWDI) #eyeballing missing data

names(allWDI)[4] <- 'unem.total'
names(allWDI)[5] <- 'unem.youth'
names(allWDI)[6] <- 'unem.youth.m'
names(allWDI)[7] <- 'GDPpc'
names(allWDI)[8] <- 'GDP'
names(allWDI)[9] <- 'GDP.gr'
names(allWDI)[10] <- 'GDPpc.gr'
names(allWDI)[11] <- 'mobile'
names(allWDI)[12] <- 'mobilep100'
names(allWDI)[13] <- 'poprur.gr'
names(allWDI)[14] <- 'popurb.gr'
names(allWDI)[15] <- 'cmort'
names(allWDI)[16] <- 'property'
names(allWDI)[17] <- 'empl.agrar'
names(allWDI)[18] <- 'gini'

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
rm(dummy, idx)

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


###################################
# MERGE 5 ### MERGE 5 ### MERGE 5 #
###################################
url <- "https://www.cia.gov/library/publications/the-world-factbook/fields/2060.html" #length of coastline
page <- getURL(url, .opts = list(ssl.verifypeer=FALSE))
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
merge5 <- merge(merge4,length,by=c("iso2c"), all.x = TRUE) #merges merge4 + coastline length
rm(length, url, page, tables, lcc)


###################################
# MERGE 6 ### MERGE 6 ### MERGE 6 #
###################################
polity <- read.csv("p4v2014.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA")) #polity iv project
polity <- polity[,c(4:5,8:10)]
#pcc <- polity$country
#polity$iso2c <- countrycode(pcc, "country.name", "iso2c")
#polity$country <- NULL
polity$polity[polity$polity==-88] <- NA
polity$polity[polity$polity==-77] <- NA
polity$polity[polity$polity==-66] <- NA
polity$polity[polity$polity==-10] <- 111
polity$polity[polity$polity==-9] <- 111
polity$polity[polity$polity==-8] <- 111
polity$polity[polity$polity==-7] <- 111
polity$polity[polity$polity==-6] <- 111
polity$polity[polity$polity==-5] <- 222
polity$polity[polity$polity==-4] <- 222
polity$polity[polity$polity==-3] <- 222
polity$polity[polity$polity==-2] <- 222
polity$polity[polity$polity==-1] <- 222
polity$polity[polity$polity==0] <- 222
polity$polity[polity$polity==1] <- 222
polity$polity[polity$polity==2] <- 222
polity$polity[polity$polity==3] <- 222
polity$polity[polity$polity==4] <- 222
polity$polity[polity$polity==5] <- 222
polity$polity[polity$polity==6] <- 333
polity$polity[polity$polity==7] <- 333
polity$polity[polity$polity==8] <- 333
polity$polity[polity$polity==9] <- 333
polity$polity[polity$polity==10] <- 333
polity$polity[polity$polity==111] <- 1
polity$polity[polity$polity==222] <- 2
polity$polity[polity$polity==333] <- 3
polity$polity <- factor(polity$polity,
                        levels = c(1,2,3),
                        labels = c("autocracy", "anocracy", "democracy"))
polity <- polity[,c(1:2,5)]
merge6 <- merge(merge5,polity,by=c("country", "year"), all.x = TRUE) #merges merge5 + polity data series (why does it only work w/ 'country' not w/ 'iso2c'?)
missmap(merge6) #eyeballing missing data
rm(polity, pcc)



###############################################
###############################################
# Analysis Socio-Economic Determinants - Lim.D.
###############################################
###############################################
#merge3 <- read.csv("merge3.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
#Africa <- read.csv("africa.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
panel <- pdata.frame(merge6, index=c("iso2c", "year")) #setting dataframe to panel data
rm(merge1, merge2, merge3, merge4, merge5)