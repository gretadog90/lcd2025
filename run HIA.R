################################################################################
# Run Health Impact Function
################################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringi)
library(data.table)

#set working directory
setwd('~/Documents/data/Lancet 2025/')
output='outputHIA/'

#read in merged mortality and exposure data
merged_data_2025<-read.csv("output/merged_data_2025.csv")
who_region<-read.csv("groupings/who_regions.csv")
merged_data_2025<-merge(merged_data_2025, who_region[,c("alpha.3", "sub.region")], 
                by.x="ISO3",by.y="alpha.3", all.x = TRUE)

hia_100m<-merged_data_2025[,c("country", "city", "ID_HDC_G0", "hdi_level", "lc_group", "sub.region",
                         "PopWeight_Peak_NDVI_2015_100m", "PopWeight_Peak_NDVI_2020_100m",
                         "PopWeight_Peak_NDVI_2021_100m", "PopWeight_Peak_NDVI_2022_100m",
                         "PopWeight_Peak_NDVI_2023_100m", "clim_region", "who_region",
                         "Population_2015_100m", "Population_2020_100m", "Longitude", "Latitude",
                         "val.2015", "upper.2015", "lower.2015","val.2020", "upper.2020", "lower.2020")]

#merge in all the other years of data that are not inluded in LCD results
data2014<-read.csv("output/data2014.csv")
data2014<-data2014[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2014_100m")]
data2016<-read.csv("output/data2016.csv")
data2016<-data2016[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2016_100m")]
data2017<-read.csv("output/data2017.csv")
data2017<-data2017[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2017_100m")]
data2018<-read.csv("output/data2018.csv")
data2018<-data2018[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2018_100m")]
data2019<-read.csv("output/data2019.csv")
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

#set the protective HR, lb and ub per .1 increase in NDVI
hr= (0.96)
hr_lb=(0.94) 
hr_ub=(0.97)

hia_100m$diff<-hia_100m$ndvi2019_2023-hia_100m$ndvi2014_2018

### 100m
#using 2020 pop and 2020 baseline mort
hia_100m$delta_mortality=hia_100m$val.2020*hia_100m$Population_2020_100m*(hia_100m$diff/.1)*hr

write.csv(hia_100m, paste0(output,"hia_100m_5yr.csv"))
