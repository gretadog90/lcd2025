#########################################################################
# Create .csvs for tables in appendix
#########################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(janitor)

####import####
#import all the yearly data files
#set working directory
setwd('~/Documents/data/Lancet 2025/LCD report')
import_dir='~/Documents/data/Lancet 2025/output/'

#import the merged dataset
lcd2025<-read.csv(paste0(import_dir, "lcd2025.csv"))

#remove one city (Kapoeta), for which shapefile is incorrect and there is no pop
lcd2025<- lcd2025[-which(lcd2025$city=='Kapoeta'),]

#remove 1km data
names(lcd2025)
data_100m<-select(lcd2025, c("Latitude", "Longitude", "ISO3", "city", "country", 
                             "lc_group", "who_region", "hdi_level", "clim_region",
                             "sub.region", ends_with("_100m"), 
                             starts_with("Avg_NDVI_"), starts_with("Peak_NDVI_"),
                             "Green_Area_2015", "GreenBlue_Area_2015",
                             "Green_Area_2020", "GreenBlue_Area_2020"))
names(data_100m) <- sub("_100m", "", names(data_100m))

#table 1. pop-weighted peak ndvi by year by LC region + global line
pw_peak<-select(data_100m, c("lc_group", starts_with("PopWeight_Peak_")))

#create a row at end with global total
pw_peak<- rbind(pw_peak, data.frame(lc_group = 'mean', t(colMeans(pw_peak[-1]))))

#collapse to lc_group
pw_peak<- pw_peak %>%
  group_by(lc_group) %>%
  summarize(
    v2015=mean(PopWeight_Peak_NDVI_2015),
    v2016=mean(PopWeight_Peak_NDVI_2016),
    v2017=mean(PopWeight_Peak_NDVI_2017),
    v2018=mean(PopWeight_Peak_NDVI_2018),
    v2019=mean(PopWeight_Peak_NDVI_2019),
    v2020=mean(PopWeight_Peak_NDVI_2020),
    v2021=mean(PopWeight_Peak_NDVI_2021),
    v2022=mean(PopWeight_Peak_NDVI_2022),
    v2023=mean(PopWeight_Peak_NDVI_2023),
    v2024=mean(PopWeight_Peak_NDVI_2024)
  )

#export
write.csv(pw_peak, 'table1.csv')
