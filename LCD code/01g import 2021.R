################################################################
# Import 2021 GEE data to create the yearly NDVI metrics
################################################################

#### Set up ####
#clear objects
rm(list = ls()) 

#load in relevant libraries
library(tidyr)
library(tidyverse)
library(dplyr)

#set working directory
setwd('~/Documents/data/Lancet 2025/2021')
output_dir="~/Documents/data/Lancet 2025/output/"

####import####
#get list of all files ending in .csv in wd
file_list = list.files(pattern="\\.csv$") 

#for each file in file list, rename to drop "2021_" and ".csv" and read in csv as object
for (i in 1:length(file_list)) assign(str_remove(str_remove(file_list[i], "2021_"),".csv"), 
                                      read.csv(file_list[i])) 

#import 2020 population (only updated once every 5 years)
pop_denom<-read.csv('~/Documents/data/Lancet 2025/2020/2020_pop_denom.csv')
pop_denom_100m<-read.csv('~/Documents/data/Lancet 2025/2020/2020_pop_denom_100m.csv')

#### Seasonal NDVI data mgmt ####
#combine all the seasons together in one long dataframe
seasons<-rbind(fall,winter,spring,summer)

#collapse to get mean and max of season mean NDVI by city
seasons<- seasons %>%
  group_by(ID_HDC_G0, city, country, UC_NM_MN, XC_ISO_LST) %>%
  summarize(Avg_NDVI_2021 = mean(mean, na.rm = TRUE), Peak_NDVI_2021=max(mean, na.rm = TRUE))

#### Pop-weighted NDVI data mgmt (1km) ####
#combine all the seasons together in one long dataframe
seasons_popw<-rbind(fall_num, winter_num, spring_num, summer_num)

#collapse to get mean and max of seasonal NDVI*pop numerator by city 
seasons_popw<- seasons_popw %>%
  group_by(ID_HDC_G0, city, country, UC_NM_MN, XC_ISO_LST) %>%
  summarize(annual_avg_num_2021 = mean(sum, na.rm = TRUE), annual_max_num_2021 = max(sum, na.rm = TRUE) )

#merge in denominator, keeping just column to match on and sum of population 
seasons_popw<-merge(seasons_popw, pop_denom[,c("ID_HDC_G0", "sum")], by="ID_HDC_G0")
names(seasons_popw)[names(seasons_popw) == "sum"] <- "Population_2020"
seasons_popw$PopWeight_Avg_NDVI_2021<-seasons_popw$annual_avg_num_2021/seasons_popw$Population_2020
seasons_popw$PopWeight_Peak_NDVI_2021<-seasons_popw$annual_max_num_2021/seasons_popw$Population_2020

#### Pop-weighted NDVI data mgmt (100m) ####
#combine all the seasons together in one long dataframe
seasons_popw_100m<-rbind(fall_num_100m, winter_num_100m, spring_num_100m, summer_num_100m)

#collapse to get mean and max of seasonal NDVI*pop numerator by city 
seasons_popw_100m<- seasons_popw_100m %>%
  group_by(ID_HDC_G0, city, country, UC_NM_MN, XC_ISO_LST) %>%
  summarize(annual_avg_num_2021_100m = mean(sum, na.rm = TRUE), 
            annual_max_num_2021_100m = max(sum, na.rm = TRUE) )

#merge in denominator, keeping just column to match on and sum of population 
seasons_popw_100m<-merge(seasons_popw_100m, pop_denom_100m[,c("ID_HDC_G0", "sum")], by="ID_HDC_G0")
names(seasons_popw_100m)[names(seasons_popw_100m) == "sum"] <- "Population_2020_100m"
seasons_popw_100m$PopWeight_Avg_NDVI_2021_100m<-seasons_popw_100m$annual_avg_num_2021_100m/seasons_popw_100m$Population_2020_100m
seasons_popw_100m$PopWeight_Peak_NDVI_2021_100m<-seasons_popw_100m$annual_max_num_2021_100m/seasons_popw_100m$Population_2020_100m

#### Combine all 2021 measures together based measures ####
data2021<-seasons %>%
  inner_join(seasons_popw) %>% 
  inner_join(seasons_popw_100m)

#get rid of a couple of unnecessary vars
data2021[c("annual_avg_num_2021", "annual_max_num_2021", "annual_avg_num_2021_100m", 
           "annual_max_num_2021_100m")] <- list(NULL) 

write.csv(data2021, paste0(output_dir, 'data2021.csv'))
