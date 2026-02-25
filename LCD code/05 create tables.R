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

####import####
#import all the yearly data files
#set working directory
setwd('~/Documents/data/Lancet 2025/output')

#import the merged dataset
lcd2025<-read.csv("lcd2025.csv")

#remove one city (Kapoeta), for which shapefile is incorrect and there is no pop
lcd2025<- lcd2025[-which(lcd2025$city=='Kapoeta'),]

#remove 1km data
names(lcd2025)
data_100m<-lcd2025 %>%
  rename(Peak_NDVI_2025_100m=Peak_NDVI_2025,
         Avg_NDVI_2025_100m=Avg_NDVI_2025) %>%
  select(c("Latitude", "Longitude", "ISO3", "city", "country", 
           "lc_group", "who_region", "hdi_level", "clim_region",
           "sub.region", ends_with("_100m"),
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
  dplyr::summarize(
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

#subset to 2025 for the following tables
current_year<-select(data_100m, c("lc_group", "who_region", "hdi_level", "clim_region",
                                                    ends_with("2025")))

#table 2. avg ndvi, peak ndvi, pop-weighted avg + peak ndvi by HDI
#collapse to hdi
hdi<- current_year %>%
  group_by(hdi_level) %>%
  summarize(
    peak_ndvi=mean(Peak_NDVI_2025),
    avg_ndvi=mean(Avg_NDVI_2025),
    popw_peak_ndvi=mean(PopWeight_Peak_NDVI_2025),
    popw_avg_ndvi=mean(PopWeight_Avg_NDVI_2025)
  )

#export
write.csv(hdi, 'table2.csv')

#table 3. avg ndvi, peak ndvi, pop-weighted avg + peak ndvi by climate region
#collapse to clim_region
clim_region<- current_year %>%
  group_by(clim_region) %>%
  summarize(
    peak_ndvi=mean(Peak_NDVI_2025),
    avg_ndvi=mean(Avg_NDVI_2025),
    popw_peak_ndvi=mean(PopWeight_Peak_NDVI_2025),
    popw_avg_ndvi=mean(PopWeight_Avg_NDVI_2025)
  )

#export
write.csv(clim_region, 'table3.csv')

#table 4. avg ndvi, peak ndvi, pop-weighted avg + peak ndvi by who_region
#collapse to who_region
who_region<- current_year %>%
  group_by(who_region) %>%
  summarize(
    peak_ndvi=mean(Peak_NDVI_2025),
    avg_ndvi=mean(Avg_NDVI_2025),
    popw_peak_ndvi=mean(PopWeight_Peak_NDVI_2025),
    popw_avg_ndvi=mean(PopWeight_Avg_NDVI_2025)
  )

#export
write.csv(who_region, 'table4.csv')

#table 3. avg ndvi, peak ndvi, pop-weighted avg + peak ndvi by lc_group
#collapse to clim_region
lc_group<- current_year %>%
  group_by(lc_group) %>%
  summarize(
    peak_ndvi=mean(Peak_NDVI_2025),
    avg_ndvi=mean(Avg_NDVI_2025),
    popw_peak_ndvi=mean(PopWeight_Peak_NDVI_2025),
    popw_avg_ndvi=mean(PopWeight_Avg_NDVI_2025)
  )

#export
write.csv(lc_group, 'table5.csv')

#generate an urban area var that will be 1-green or blue area
lcd2025$Blue_Area_2015<-lcd2025$GreenBlue_Area_2015-lcd2025$Green_Area_2015
lcd2025$Blue_Area_2020<-lcd2025$GreenBlue_Area_2020-lcd2025$Green_Area_2020
hist(lcd2025$Blue_Area_2020)

blue_area_2020<-select(lcd2025, c("Latitude", "Longitude", "ISO3", "city", "country", 
                                  "lc_group", "who_region", "hdi_level", "clim_region",
                                  "Blue_Area_2020"))
write.csv(blue_area_2020, 'blue_area.csv')