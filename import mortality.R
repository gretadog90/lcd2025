################################################################################
# Import mortality data and do some data cleaning/mgmt for merge with nature data
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
import_dir='GBD mortality/'
output='output/'

####import####
gbd_mort<-read.csv(paste0(import_dir, "IHME-GBD_2021_DATA-b41c2546-1.csv"))
merged_data_2025<-read.csv(paste0(output, "merged_data_2025.csv"))

####data mgmt####
# a lot of manual replacing needed to match location_name to country/region in data
#start by removing special characters
gbd_mort$location_name<-stri_trans_general(gbd_mort$location_name, "Latin-ASCII")

#find which names don't match and edit gbd names to match lcd universe
merged_data_2025$country[!merged_data_2025$country %in% gbd_mort$location_name]
gbd_mort$location_name[gbd_mort$location_name=="Russian Federation"]<-"Russia"
gbd_mort$location_name[gbd_mort$location_name=="Iran (Islamic Republic of)"]<-"Iran"
gbd_mort$location_name[gbd_mort$location_name=="United Republic of Tanzania"]<-"Tanzania"
gbd_mort$location_name[gbd_mort$location_name=="United States of America"]<-"United States"
gbd_mort$location_name[gbd_mort$location_name=="Bolivia (Plurinational State of)"]<-"Bolivia"
gbd_mort$location_name[gbd_mort$location_name=="Republic of Moldova"]<-"Moldova"
gbd_mort$location_name[gbd_mort$location_name=="Turkiye"]<-"Turkey" 
gbd_mort$location_name[gbd_mort$location_name=="Lao People's Democratic Republic"]<-"Laos"
gbd_mort$location_name[gbd_mort$location_name=="Viet Nam"]<-"Vietnam"
gbd_mort$location_name[gbd_mort$location_name=="Congo"]<-"Republic of the Congo"
gbd_mort$location_name[gbd_mort$location_name=="Venezuela (Bolivarian Republic of)"]<-"Bolivarian Republic of Venezuela"
gbd_mort$location_name[gbd_mort$location_name=="Brunei Darussalam"]<-"Brunei"
gbd_mort$location_name[gbd_mort$location_name=="North Macedonia"]<-"Macedonia"
gbd_mort$location_name[gbd_mort$location_name=="Cabo Verde"]<-"Cape Verde"
gbd_mort$location_name[gbd_mort$location_name=="Czechia"]<-"Czech Republic"

#check if location_name is unique before reshape wide
gbd_mort<-add_count(gbd_mort, location_name, year)
gbd_mort$location_name[gbd_mort$n>1]

#there are a few location_names that are duplicated. Two are regions w/ identical mort values
# (north africa and middle east, south asia) but diff location ids
#two are both countries and sub-national values (georgia and mexico) 
# and one sub-national for both India and Pakistan (punjab). will drop one of the dup regions 
# and rename rest to be clear
mort<-gbd_mort[!gbd_mort$location_id %in% c(137, 158),] #drop dup region entries by location_id
mort$location_name[mort$location_id==533]<-"Georgia [US]"
mort$location_name[mort$location_id==4657]<-"Mexico [Mexico]"
mort$location_name[mort$location_id==4867]<-"Punjab [India]"
mort$location_name[mort$location_id==53620]<-"Punjab [Pakistan]"

#subset to just the needed vars
mort<-mort[,c("location_name", "year", "val", "upper", "lower")]

#rehape wide so that theres just one row per country and separate columns for 2015 and 2020 estimates
mort<- reshape(mort, idvar = "location_name", timevar = "year", direction = "wide")

#mort rate is expressed as # deaths per 100,000. divide by 100,000 to get rate per 1 person
mort$constant<-100000
mort<- mort %>%
  mutate(across(starts_with("val")|starts_with("lower")|starts_with("upper"),
       ~ . / constant))
             
#merge exposure and pop data
hia<-merge(merged_data_2025, mort, by.x="country",by.y="location_name")

#set the protective HR, lb and ub per .1 increase in NDVI
phr= (1-0.96)/.1
phr_lb=(1-0.94)/.1 
phr_ub=(1-0.97)/.1

# hia 2015, 2020, 2023. all with .1 increase in NDVI
#using 2015 pop and 2015 baseline mort
hia$e_2020_2015=hia$val.2015*hia$Population_2015_100m*(hia$PopWeight_Peak_NDVI_2020_100m-hia$PopWeight_Peak_NDVI_2015_100m)*phr

#using 2020 pop and 2020 baseline mort
hia$e_2020_2020=hia$val.2020*hia$Population_2020_100m*(hia$PopWeight_Peak_NDVI_2020_100m-hia$PopWeight_Peak_NDVI_2015_100m)*phr

#run hia 2015->2023
#using 2020 pop and 2020 baseline mort
hia$e_2023_2020=hia$val.2020*hia$Population_2020_100m*(hia$PopWeight_Peak_NDVI_2023_100m-hia$PopWeight_Peak_NDVI_2020_100m)*phr

write.csv(hia, "hia.csv")
#14 countries for which sub-national mortality rates are available
merged_data_2025$city[merged_data_2025$country %in% 
                        c("South Africa", "Kenya", "Ethiopia", "Indonesia", "Pakistan",
                          "India", "Iran", "Brazil", "Mexico", "United Kingdom",
                          "Sweden", "Norway", "United States", "Japan")]
#405 cities that need a re-assignment to a sub-national region

#delete excess vars and save
merged_data_2025[,c("X", "X.1")]<- NULL #get rid of ids that are auto generated



