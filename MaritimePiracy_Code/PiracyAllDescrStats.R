########################
# Maritime Piracy - Descriptive Statistics
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

piracy <- read.csv("piracy.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = c("", "NA"))
piracy$X <- NULL
summary(piracy$iso2c)
class(piracy$iso2c)
piracy$iso2c <- as.character(piracy$iso2c)
piracy$iso2c[is.na(piracy$iso2c)] <- "NA"


#ten <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE, na.strings = c("", "NA"))

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


sjp.setTheme(theme = "scatter",
             geom.label.size = 2.5,
             geom.label.color = "navy",
             axis.textsize = .8,
             axis.title.size = .9)
sjp.frq(piracy$incidents,
        title = "Fig. 1 - Frequency of Pirate Attacks 1993-2014",
        geom.colors = "darkorange",
        sort.frq = "none",
        axisTitle.x = "Incidents of Piracy",
        axisTitle.y = "Country-years",
        showPercentageValues = FALSE,
        coord.flip = FALSE)


Africa <- piracy[which(piracy$continent=="Africa"),]
summary(Africa$incidents)
table(Africa$incidents)
mean(Africa$incidents)
var(Africa$incidents)

MENA <- piracy[which(piracy$continent=="MENA"),]
table(MENA$incidents)
summary(MENA$incidents)
mean(MENA$incidents)
var(MENA$incidents)

SAsia <- piracy[which(piracy$continent=="SouthAsia"),]
table(SAsia$incidents)
summary(SAsia$incidents)
mean(SAsia$incidents)
var(SAsia$incidents)

EAsia <- piracy[which(piracy$continent=="EastAsia"),]
table(EAsia$incidents)
summary(EAsia$incidents)
mean(EAsia$incidents)
var(EAsia$incidents)

Asia <- piracy[which(piracy$continent=="EastAsia"|piracy$continent=="SouthAsia"),]
table(Asia$incidents)
summary(Asia$incidents)
mean(Asia$incidents)
var(Asia$incidents)

ROW <- piracy[which(piracy$continent=="ROW"),]
table(ROW$incidents)
summary(ROW$incidents)
mean(ROW$incidents)
var(ROW$incidents)

sum(piracy$incidents)
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
        axisTitle.x = "Incidents of Piracy",
        axisTitle.y = "Country-years",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(MENA$incidents,
        title = "Fig. 2b - MENA 1993-2014",
        geom.colors = "chocolate4",
        sort.frq = "none",
        axisTitle.x = "Incidents of Piracy",
        axisTitle.y = "Country-years",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(Asia$incidents,
        title = "Fig. 2c - Asia 1993-2014",
        geom.colors = "mediumspringgreen",
        sort.frq = "none",
        axisTitle.x = "Incidents of Piracy",
        axisTitle.y = "Country-years",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(ROW$incidents,
        title = "Fig. 2d - ROW 1993-2014",
        geom.colors = "deepskyblue2",
        sort.frq = "none",
        axisTitle.x = "Incidents of Piracy",
        axisTitle.y = "Country-years",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
