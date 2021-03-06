########################
# Maritime Piracy - Descriptive Statistics - Territorial waters vs high sea
#######################
library(ggplot2)
library(sjPlot)
library(rworldmap)
library(rworldxtra)
library(reshape2)
library(scales)

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd(""),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

ten <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE, na.strings = c("", "NA"))

ten$territorial_water_status <- gsub("\\(.*","",ten$territorial_water_status)
ten$territorial_water_status <- gsub("[^a-zA-Z0-9]","",ten$territorial_water_status)

ten$Status <- "Territorial"
ten$Status[which(ten$territorial_water_status=="InternationaWaters")] <- "International"
ten$Status[which(ten$id==6141)] <- "Unknown"
ten$Status[which(ten$id==6127)] <- "Unknown"
ten$Status[which(ten$id==6151)] <- "Unknown"
ten$Status[which(ten$id==6135)] <- "Unknown"
ten$Status[which(ten$id==6142)] <- "Unknown"
ten$Status[which(ten$id==6153)] <- "Unknown"
ten$Status[which(ten$id==6163)] <- "Unknown"
ten$Status[which(ten$id==6242)] <- "Unknown"
ten$Status[which(ten$id==6205)] <- "Unknown"
ten$Status[which(ten$id==6247)] <- "Unknown"
ten$Status[which(ten$id==6263)] <- "Unknown"
#ten$Status[ten$Status=="Territorial Water"] <- 1
#ten$Status[ten$Status=="International Water"] <- 2
#ten$Status[ten$Status=="Unkown"] <- 3


#barplot for frequency of attacks
a <- ggplot(ten, aes(x=Status, y=(..count..)/sum(..count..), fill=Status))
a + geom_bar(stat="count") +
  scale_y_continuous(labels=percent) +
  xlab("Water Status") +
  ylab("Percent") +
  ggtitle("Piracy Attacks by Water Status 1993 - 2014") +
  theme(legend.text=element_text(size=25))
