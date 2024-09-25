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

#### merge all the yearly greenspace data sets together ####
lcd2025 <- data2015 %>%
  inner_join(data2020) %>% 
  inner_join(data2021) %>%
  inner_join(data2022) %>%
  inner_join(data2023)

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
# for all the pop-weighted peak NDVI variables (years and 100m versions), 
# create greenness indicator
lcd2025<- lcd2025 %>%
  mutate(across(starts_with('PopWeight_Peak_NDVI_'),
                ~case_when( . >=0.7 ~ 'Exceptionally High',
                           . >= 0.6 & . <0.7 ~ 'Very High',
                           . >= 0.5 & . <0.6 ~ 'High',
                           . >= 0.4 & . <0.5 ~ 'Moderate',
                           . >= 0.3 & . <0.4 ~ 'Low',
                           . >= 0.2 & . <0.3 ~ 'Very Low',
                           . <= 0.2 ~ 'Exceptionally Low',
                           TRUE ~ 'NA'), 
                .names = '{sub("PopWeight_Peak_NDVI_", "indicator_", .col)}'))
                                  
lcd2025$clim_region <- factor(lcd2025$clim_region,
                                  levels = c(1,2,3,4,5),
                                  labels = c("Tropical", "Arid", "Temperate", "Continental", "Polar"))

#################
# Check numbers #
#################
country_count<-unique(lcd2025$country) #174
city_count<-unique(lcd2025$city) #1042

write.csv(lcd2025, "merged_data_2025.csv")
