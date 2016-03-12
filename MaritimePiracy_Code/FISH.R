##FISH


#Import the dataset about piracy incidentss into your wd 
#Call libraries we need for the project, make sure you have them installed
library(base)
library(rio) #swiss army knife for imports
library(plyr) #command ddply
library(dplyr) #data wrangling
library(tidyr) #data wrangling
library(car) #scatterplots 
library(httr) #scraping from http sites
library(XML) #tool for generating XML file
library(WDI) #scraping Data from the World Bank
library(countrycode) #provides world bank country codes 
library(Amelia) #eyeballing missing values
library(tidyr) #reshaping
library(stringr)
library(Rcpp)
library(XML)
library(RCurl)
library(reshape2)

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracyWrangling/Data"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

FISH <- read.csv("figis_guestnull3.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("+NA"))
FISH$S_1990 <- NULL
FISH$S_1991 <- NULL
FISH$S_1992 <- NULL
FISH$S_1993 <- NULL
FISH$S_1994 <- NULL
FISH$S_1995 <- NULL
FISH$S_1996 <- NULL
FISH$S_1997 <- NULL
FISH$S_1998 <- NULL
FISH$S_1999 <- NULL
FISH$S_2000 <- NULL
FISH$S_2001 <- NULL
FISH$S_2002 <- NULL
FISH$S_2003 <- NULL
FISH$S_2004 <- NULL
FISH$S_2005 <- NULL
FISH$S_2006 <- NULL
FISH$S_2007 <- NULL
FISH$S_2008 <- NULL
FISH$S_2009 <- NULL
FISH$S_2010 <- NULL
FISH$S_2011 <- NULL
FISH$S_2012 <- NULL
FISH$S_2013 <- NULL
FISH$S_2014 <- NULL
#FISH$Species <- NULL
#FISH$Scientific.name <- NULL

#cc <- na.omit(FISH)$Land.Area
#FISH$iso2c <- countrycode(cc, "country.name", "iso2c")
#rm(cc)

FISH$ID <- as.character(paste(FISH$Land.Area, FISH$Unit, sep = "-"))
FISH <- FISH[,c(28,1:27)]

FISH$Land.Area <- NULL
FISH$Ocean.Area <- NULL
FISH$Unit <- NULL

FISH2 <- t(FISH)
FISH3 <- as.data.frame(FISH2)
#FISH3$year <- rownames(FISH3)

write.csv((FISH3 <- as.data.frame(FISH2)), file = "FISH3.csv")
FISH3 <- read.csv("FISH3.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("+NA"))

for (i in 1:25) {
  FISH3[, i] %>%
    as.character()
}
rm(i)

colnames(FISH3) = FISH3[1, ] # the first row will be the header
FISH3 = FISH3[-1, ]
names(FISH3)[1] <- 'year'
FISH3$year <- substring(FISH3$year, 2) #delete first letter

write.csv(FISH3, file = "FISH4.csv")

FISH4 <- read.csv("FISH4.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("+NA"))
FISH4$X <- NULL

library(reshape2)
write.csv((FISH5 <- melt(FISH4, id.vars=c("year"))), file = "FISH5.csv")
