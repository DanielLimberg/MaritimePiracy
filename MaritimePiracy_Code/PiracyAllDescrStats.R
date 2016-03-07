########################
# Maritime Piracy - Descriptive Statistics
#######################

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd(""),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

piracy <- read.csv("piracy.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, na.strings = c("", "NA"))
piracy$X <- NULL

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
library(sjPlot)
sjp.setTheme(theme = "scatter",
             geom.label.size = 2.5,
             geom.label.color = "navy",
             axis.textsize = .8,
             axis.title.size = .9)
sjp.frq(piracy$incidents,
        title = "Fig. 1 - Frequency of Pirate Attacks 1993-2014",
        geom.colors = "darkorange",
        sort.frq = "none",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
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



sjp.setTheme(theme = "scatter",
             geom.label.size = 2.5,
             geom.label.color = "navy",
             axis.textsize = .8,
             axis.title.size = .9)
sjp.frq(Africa$incidents,
        title = "Fig. 2a - Africa 1993-2014",
        geom.colors = "firebrick1",
        sort.frq = "none",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(MENA$incidents,
        title = "Fig. 2b - MENA 1993-2014",
        geom.colors = "chocolate4",
        sort.frq = "none",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(Asia$incidents,
        title = "Fig. 2c - Asia 1993-2014",
        geom.colors = "mediumspringgreen",
        sort.frq = "none",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
        showPercentageValues = FALSE,
        coord.flip = FALSE)
sjp.frq(ROW$incidents,
        title = "Fig. 2d - ROW 1993-2014",
        geom.colors = "deepskyblue2",
        sort.frq = "none",
        axisTitle.x = "Incidents of Piracy per country-year",
        axisTitle.y = "Frequency",
        showPercentageValues = FALSE,
        coord.flip = FALSE)

sum(piracy$incidents)
Afsum <- sum(Africa$incidents)
MEsum <- sum(MENA$incidents)
Assum <- sum(Asia$incidents)
ROWsum <- sum(ROW$incidents)

farben = c("firebrick1", "chocolate4", "mediumspringgreen", "deepskyblue2")

slices <- c(Afsum, MEsum, Assum, ROWsum) 
lbls <- c("Africa", "MENA", "Asia", "ROW")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=farben,
    main="Fig. 3 - Share of Incidents of Maritime Piracy 1993-2014")

piracy$continent2 <- piracy$continent
piracy$incidents2 <- piracy$incidents
piracy$continent2 <- as.character(piracy$continent2)
piracy$continent2[which(piracy$continent=="EastAsia")] <- "Asia"
piracy$continent2[which(piracy$continent=="SouthAsia")] <- "Asia"
piracy$continent2 <- as.factor(piracy$continent2)
aggrtpi <- dcast(piracy, continent2 + year ~ incidents, sum) #p317 R for Dummies
aggrtpi$ytotal <- rowSums(aggrtpi[,3:67])

library(ggplot2)
p <- ggplot(data = aggrtpi, aes(x = year, y = ytotal, group = continent2, color = continent2)) + geom_line() + ggtitle("Fig. 4 - Incidents of Maritime Piracy 1993-2014") + labs(x = "Year", y = "No. of Incidents")
p + scale_colour_discrete(name  ="Region", labels=c("Africa", "Asia", "MENA", "ROW"))





summary(NB1 <- glm.nb(incidents ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel))
summary(Ptest <- glm(incbinary ~ cmort + FD + DD + pop.gr + ((polity2)^2) + GDPpc + WTI + unem.total + log(coastkm) + battlelow, data = panel, family = "poisson"))
X <- 2*(logLik(NB1) - logLik(Ptest))
list(X)
pchisq(X, df = 1, lower.tail=FALSE)

rm(NB1, Ptest, X)
