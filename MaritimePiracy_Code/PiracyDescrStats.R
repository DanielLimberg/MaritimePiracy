########################
# Maritime Piracy - Descriptive Statistics
#######################

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracyWrangling/Data"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/MaritimePiracy/MaritimePiracy_Data"),silent=TRUE)
getwd()

# Dynamical Link to first R script file
source("MergeAll.R")


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