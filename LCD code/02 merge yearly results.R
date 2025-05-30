#########################################################################
# Merge yearly metrics and WHO, HDI, and climate region classifications.
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
import_dir='~/Documents/data/Lancet 2025/groupings/'

# ger list of all the files ending in .csv
file_list = list.files(pattern="\\.csv$") 

#for each file in file list, rename to drop "2020_" and ".csv" and read in csv as object
for (i in 1:length(file_list)) assign(str_remove(file_list[i],".csv"), 
                                      read.csv(file_list[i])) 

#import all the outside data sources to be merged (WHO regions, HDI, etc.)
capitals<-read.csv(paste0(import_dir, "Urban Green Spaces Capitals.csv"))
country_lcd<-read.csv(paste0(import_dir,"country_names_groupings.csv"))
clim_region<-read.csv(paste0(import_dir, "clim_reg_cities.csv"))
who_region<-read.csv(paste0(import_dir, "who_regions.csv"))

#### merge all the yearly greenspace data sets together ####
lcd2025 <- data2015 %>%
  inner_join(data2016) %>% 
  inner_join(data2017) %>%
  inner_join(data2018) %>%
  inner_join(data2019) %>% 
  inner_join(data2020) %>%
  inner_join(data2021) %>%
  inner_join(data2022) %>%
  inner_join(data2023) %>%
  inner_join(data2024)

#merge to capitals data set
#"Sio TomA (should be Sao Tome) not in these external datasets. addressed below
lcd2025$city[lcd2025$city=="Sio TomA"]<-"Sao Tome"
lcd2025$city[!lcd2025$city %in% capitals$UC_NM_LST] 
lcd2025<-merge(lcd2025, capitals[,c("UC_NM_LST", "Capital_city", "Latitude", "Longitude")], 
               by.x="city",by.y="UC_NM_LST", all.x = TRUE)

#merge to country_lcd data set
#there are three countries that don't match due to accents, editing here
lcd2025$country[!lcd2025$country %in% country_lcd$Country_edit]
lcd2025$country[lcd2025$XC_ISO_LST=="CIV"]<-"Cote d'Ivoire"
lcd2025$country[lcd2025$country=="Taiwan"]<-"China"
lcd2025$country[lcd2025$XC_ISO_LST=="STP"]<-"Sao Tome and Principe"

#now merge
lcd2025<-merge(lcd2025, country_lcd[,c("Country_edit", "ISO3", "lc_group", "who_region", "hdi_level")], 
               by.x="country",by.y="Country_edit")

#merge to clim_region data set
#one city with incorrect edit to remove accent
lcd2025$city[!lcd2025$city %in% clim_region$UC_NM_LST] 
clim_region$UC_NM_LST[clim_region$UC_NM_LST=="Beltm"]<-"Belem"

lcd2025<-merge(lcd2025, clim_region[,c("UC_NM_LST", "clim_region")], 
               by.x="city",by.y="UC_NM_LST", all.x = TRUE)

#one city (Sao Tome) not in these a couple of the data sets. adding info manually
lcd2025$Capital_city[lcd2025$city=="Sao Tome"]<-1 #yes, capital city
lcd2025$clim_region[lcd2025$city=="Sao Tome"]<-1 #tropical wet and dry
lcd2025$lc_group[lcd2025$city=="Sao Tome"]<-"Africa" #Africa
lcd2025$hdi_level[lcd2025$city=="Sao Tome"]<-"Medium" #0.618 (medium)
lcd2025$who_region[lcd2025$city=="Sao Tome"]<-"Africa" #Africa
lcd2025$Latitude[lcd2025$city=="Sao Tome"]<-0.336111 #lat
lcd2025$Longitude[lcd2025$city=="Sao Tome"]<-6.730556 #lon

#### Create variables ####
lcd2025$clim_region <- factor(lcd2025$clim_region,
                                  levels = c(1,2,3,4,5),
                                  labels = c("Tropical", "Arid", "Temperate", "Continental", "Polar"))

#add on WHO sub region labels
lcd2025<-merge(lcd2025, who_region[,c("alpha.3", "sub.region")], 
               by.x="ISO3",by.y="alpha.3", all.x = TRUE)

#################
# Check numbers #
#################
country_count<-unique(lcd2025$country) #174
city_count<-unique(lcd2025$city) #1042

write.csv(lcd2025, "lcd2025.csv")

summary(lcd2025$PopWeight_Peak_NDVI_2015)
summary(lcd2025$PopWeight_Peak_NDVI_2015_100m)
summary(lcd2025$PopWeight_Peak_NDVI_2016)
summary(lcd2025$PopWeight_Peak_NDVI_2016_100m)
summary(lcd2025$PopWeight_Peak_NDVI_2017)
summary(lcd2025$PopWeight_Peak_NDVI_2017_100m)
summary(lcd2025$PopWeight_Peak_NDVI_2018)
summary(lcd2025$PopWeight_Peak_NDVI_2018_100m)
summary(lcd2025$PopWeight_Peak_NDVI_2019)
summary(lcd2025$PopWeight_Peak_NDVI_2019_100m)
summary(lcd2025$PopWeight_Peak_NDVI_2020)
summary(lcd2025$PopWeight_Peak_NDVI_2020_100m)
summary(lcd2025$PopWeight_Peak_NDVI_2021)
summary(lcd2025$PopWeight_Peak_NDVI_2021_100m)
summary(lcd2025$PopWeight_Peak_NDVI_2022)
summary(lcd2025$PopWeight_Peak_NDVI_2022_100m)
summary(lcd2025$PopWeight_Peak_NDVI_2023)
summary(lcd2025$PopWeight_Peak_NDVI_2023_100m)
summary(lcd2025$PopWeight_Peak_NDVI_2024)
summary(lcd2025$PopWeight_Peak_NDVI_2024_100m)