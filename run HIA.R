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

#set working directory
setwd('~/Documents/data/Lancet 2025/')
output='outputHIA/'

#read in merged mortality and exposure data
merged_data_2025<-read.csv("output/merged_data_2025.csv")
hia_100m<-merged_data_2025[,c("country", "city", "ID_HDC_G0", "hdi_level", "lc_group",
                         "PopWeight_Peak_NDVI_2015_100m", "PopWeight_Peak_NDVI_2020_100m","PopWeight_Peak_NDVI_2023_100m",
                         "Population_2015_100m", "Population_2020_100m", "Longitude", "Latitude",
                         "val.2015", "upper.2015", "lower.2015","val.2020", "upper.2020", "lower.2020")]
hia<-merged_data_2025[,c("country", "city", "ID_HDC_G0", "hdi_level", "lc_group",
                              "PopWeight_Peak_NDVI_2015", "PopWeight_Peak_NDVI_2020","PopWeight_Peak_NDVI_2023",
                              "Population_2015", "Population_2020", "Longitude", "Latitude",
                              "val.2015", "upper.2015", "lower.2015","val.2020", "upper.2020", "lower.2020")]

#set the protective HR, lb and ub per .1 increase in NDVI
phr= (1-0.96)/.1
phr_lb=(1-0.94)/.1 
phr_ub=(1-0.97)/.1

### 100m
# hia 2015-2020. 4 equations to seperate out drivers of HIA
#using 2015 pop and 2015 baseline mort
hia_100m$e2020_all2015=hia_100m$val.2015*hia_100m$Population_2015_100m*(hia_100m$PopWeight_Peak_NDVI_2020_100m-hia_100m$PopWeight_Peak_NDVI_2015_100m)*phr

#using 2015 pop and 2020 baseline mort
hia_100m$e2020_pop2015=hia_100m$val.2020*hia_100m$Population_2015_100m*(hia_100m$PopWeight_Peak_NDVI_2020_100m-hia_100m$PopWeight_Peak_NDVI_2015_100m)*phr

#using 2020 pop and 2015 baseline mort
hia_100m$e2020_mort2015=hia_100m$val.2015*hia_100m$Population_2020_100m*(hia_100m$PopWeight_Peak_NDVI_2020_100m-hia_100m$PopWeight_Peak_NDVI_2015_100m)*phr

#using 2020 pop and 2020 baseline mort
hia_100m$e2020_all2020=hia_100m$val.2020*hia_100m$Population_2020_100m*(hia_100m$PopWeight_Peak_NDVI_2020_100m-hia_100m$PopWeight_Peak_NDVI_2015_100m)*phr

# hia 2020-2023. 1 equations because shared estimate of pop and mort
#using 2020 pop and 2020 baseline mort
hia_100m$e_2023_2020=hia_100m$val.2020*hia_100m$Population_2020_100m*(hia_100m$PopWeight_Peak_NDVI_2023_100m-hia_100m$PopWeight_Peak_NDVI_2020_100m)*phr

write.csv(hia_100m, paste0(output,"hia_100m.csv"))

### 1km
# hia 2015-2020. 4 equations to seperate out drivers of HIA
#using 2015 pop and 2015 baseline mort
hia$e2020_all2015=hia$val.2015*hia$Population_2015*(hia$PopWeight_Peak_NDVI_2020-hia$PopWeight_Peak_NDVI_2015)*phr

#using 2015 pop and 2020 baseline mort
hia$e2020_pop2015=hia$val.2020*hia$Population_2015*(hia$PopWeight_Peak_NDVI_2020-hia$PopWeight_Peak_NDVI_2015)*phr

#using 2020 pop and 2015 baseline mort
hia$e2020_mort2015=hia$val.2015*hia$Population_2020*(hia$PopWeight_Peak_NDVI_2020-hia$PopWeight_Peak_NDVI_2015)*phr

#using 2020 pop and 2020 baseline mort
hia$e2020_all2020=hia$val.2020*hia$Population_2020*(hia$PopWeight_Peak_NDVI_2020-hia$PopWeight_Peak_NDVI_2015)*phr

# hia 2020-2023. 1 equations because shared estimate of pop and mort
#using 2020 pop and 2020 baseline mort
hia$e_2023_2020=hia$val.2020*hia$Population_2020*(hia$PopWeight_Peak_NDVI_2023-hia$PopWeight_Peak_NDVI_2020)*phr

write.csv(hia, paste0(output,"hia.csv"))