###

# compare % urban from landcover dataset to NDVI 2015 and 2020

###

#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(dplyr)

#set working directory
setwd('~/Documents/data/Lancet 2025/output')

merged_data_2025<-read.csv("merged_data_2025.csv")
merged_data_2025<-merged_data_2025[!is.na(merged_data_2025$PopWeight_Peak_NDVI_2015_100m),]

merged_data_2025$urban_fraction2015<-1-merged_data_2025$GreenBlue_Area_2015
summary(merged_data_2025$urban_fraction2015)

merged_data_2025$urban_fraction2020<-1-merged_data_2025$GreenBlue_Area_2020
summary(merged_data_2025$urban_fraction2020)

merged_data_2025$change_urban_fraction<-merged_data_2025$urban_fraction2020-merged_data_2025$urban_fraction2015
merged_data_2025$change_NDVI<-merged_data_2025$PopWeight_Peak_NDVI_2020_100m-merged_data_2025$PopWeight_Peak_NDVI_2015_100m

cor(merged_data_2025$change_urban_fraction, merged_data_2025$change_NDVI, method="pearson")


hia_100m<-merged_data_2025[,c("country", "city", "ID_HDC_G0",
                              "PopWeight_Peak_NDVI_2015_100m", "PopWeight_Peak_NDVI_2020_100m",
                              "PopWeight_Peak_NDVI_2021_100m", "PopWeight_Peak_NDVI_2022_100m",
                              "PopWeight_Peak_NDVI_2023_100m", 
                              "Population_2015_100m", "Population_2020_100m", 
                              "urban_fraction2015", "urban_fraction2020", "change_urban_fraction", "change_NDVI")]

#merge in all the other years of data that are not inluded in LCD results
data2014<-read.csv("data2014.csv")
data2014<-data2014[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2014_100m")]
data2016<-read.csv("data2016.csv")
data2016<-data2016[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2016_100m")]
data2017<-read.csv("data2017.csv")
data2017<-data2017[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2017_100m")]
data2018<-read.csv("data2018.csv")
data2018<-data2018[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2018_100m")]
data2019<-read.csv("data2019.csv")
data2019<-data2019[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2019_100m")]

hia_100m <- hia_100m %>%
  inner_join(data2014) %>% 
  inner_join(data2016) %>% 
  inner_join(data2017) %>% 
  inner_join(data2018) %>% 
  inner_join(data2019)

#create a 2014-8 and 2019-2023 5-year pop-weighted peak-season avg NDVI 
hia_100m$ndvi2014_2018 <- rowMeans(hia_100m[,c("PopWeight_Peak_NDVI_2014_100m", "PopWeight_Peak_NDVI_2015_100m", 
                                               "PopWeight_Peak_NDVI_2016_100m", "PopWeight_Peak_NDVI_2017_100m", 
                                               "PopWeight_Peak_NDVI_2018_100m")])
hia_100m$ndvi2019_2023 <- rowMeans(hia_100m[,c("PopWeight_Peak_NDVI_2019_100m", "PopWeight_Peak_NDVI_2020_100m", 
                                               "PopWeight_Peak_NDVI_2021_100m",  "PopWeight_Peak_NDVI_2022_100m", 
                                               "PopWeight_Peak_NDVI_2023_100m")])
hia_100m$NDVI_diff<-hia_100m$ndvi2019_2023-hia_100m$ndvi2014_2018
cor(hia_100m$change_urban_fraction, hia_100m$NDVI_diff, method="pearson")


