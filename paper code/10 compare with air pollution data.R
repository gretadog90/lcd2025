#########################################################################
# Compare results to 10,000 cities air pollution data
#########################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(tidytab)
library(ggpubr)
library(egg)
library(ggridges)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#load in data
airpoll<-read.csv("unified_data_SR-v4.csv")
hia<-read.csv("outputHIA/hia_100m_5yr.csv")

#keep just years 2014-2019 of air pollution data to match with our analysis
table(airpoll$Year)
airpoll<-airpoll[airpoll$Year %in% c(2014, 2015, 2016, 2017, 2018, 2019),]

#reshape wide so that each row is a city 
#subset first
airpoll<-airpoll%>%
  select(ID, Year, Pw_PM, Pw_O3, Pw_NO2, Rates_PM, Rates_O3, Rates_NO2)
airpoll<-reshape(airpoll, idvar = "ID", timevar = "Year", direction = "wide")

#get rid of one city with missing data
hia<-hia[!is.na(hia$ndvi2019_2023),]
#flip sign on delta_mort so that negative values mean less deaths and postive more
#and change to per 100000
hia$delta_mortality<-(hia$delta_mortality/hia$Population_2020_100m)*-100000

#merge the two datasets together
compare<-merge(hia, airpoll,
               by.x="ID_HDC_G0",by.y="ID", all.x = TRUE)
pm_compare<-subset(compare, !is.na(compare$Pw_PM.2014))

#correlate NDVI, PM, O3, and NO2 
print(cor(pm_compare$PopWeight_Peak_NDVI_2014_100m,pm_compare$Pw_PM.2014)) 
print(cor(pm_compare$PopWeight_Peak_NDVI_2015_100m,pm_compare$Pw_PM.2015)) 
print(cor(pm_compare$PopWeight_Peak_NDVI_2016_100m,pm_compare$Pw_PM.2016)) 
print(cor(pm_compare$PopWeight_Peak_NDVI_2017_100m,pm_compare$Pw_PM.2017)) 
print(cor(pm_compare$PopWeight_Peak_NDVI_2018_100m,pm_compare$Pw_PM.2018)) 
print(cor(pm_compare$PopWeight_Peak_NDVI_2019_100m,pm_compare$Pw_PM.2019)) 

o3_compare<-subset(compare, !is.na(compare$Pw_O3.2014))

#correlate NDVI, PM, O3, and NO2 
print(cor(o3_compare$PopWeight_Peak_NDVI_2014_100m,o3_compare$Pw_O3.2014)) 
print(cor(o3_compare$PopWeight_Peak_NDVI_2015_100m,o3_compare$Pw_O3.2015)) 
print(cor(o3_compare$PopWeight_Peak_NDVI_2016_100m,o3_compare$Pw_O3.2016)) 
print(cor(o3_compare$PopWeight_Peak_NDVI_2017_100m,o3_compare$Pw_O3.2017)) 
print(cor(o3_compare$PopWeight_Peak_NDVI_2018_100m,o3_compare$Pw_O3.2018)) 
print(cor(o3_compare$PopWeight_Peak_NDVI_2019_100m,o3_compare$Pw_O3.2019)) 

no2_compare<-subset(compare, !is.na(compare$Pw_NO2.2014))

#correlate NDVI, PM, O3, and NO2 
print(cor(no2_compare$PopWeight_Peak_NDVI_2014_100m,no2_compare$Pw_NO2.2014)) 
print(cor(no2_compare$PopWeight_Peak_NDVI_2015_100m,no2_compare$Pw_NO2.2015)) 
print(cor(no2_compare$PopWeight_Peak_NDVI_2016_100m,no2_compare$Pw_NO2.2016)) 
print(cor(no2_compare$PopWeight_Peak_NDVI_2017_100m,no2_compare$Pw_NO2.2017)) 
print(cor(no2_compare$PopWeight_Peak_NDVI_2018_100m,no2_compare$Pw_NO2.2018)) 
print(cor(no2_compare$PopWeight_Peak_NDVI_2019_100m,no2_compare$Pw_NO2.2019)) 

#get change in mort for each of pollutants
pm_compare$delta_mort_pm=pm_compare$Rates_PM.2019-pm_compare$Rates_PM.2014
o3_compare$delta_mort_o3=o3_compare$Rates_O3.2019-o3_compare$Rates_O3.2014
no2_compare$delta_mort_no2=no2_compare$Rates_NO2.2019-no2_compare$Rates_NO2.2014

print(cor(pm_compare$delta_mortality,pm_compare$delta_mort_pm)) 
print(cor(o3_compare$delta_mortality,o3_compare$delta_mort_o3)) 
print(cor(no2_compare$delta_mortality,no2_compare$delta_mort_no2)) 


