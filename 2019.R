################################################################
# Import 2019 GEE data to create the yearly NDVI metrics
################################################################

#### Set up ####
#clear objects
rm(list = ls()) 

#load in relevant libraries
library(tidyr)
library(tidyverse)
library(dplyr)
detach(package:plyr)

#set working directory
setwd('~/Documents/data/Lancet 2025/2019')
output_dir="/Users/gretam/Documents/data/Lancet 2025/output/"

####import####
#get list of all files ending in .csv in wd
file_list = list.files(pattern="\\.csv$") 

#for each file in file list, rename to drop "2019_" and ".csv" and read in csv as object
for (i in 1:length(file_list)) assign(str_remove(str_remove(file_list[i], "2019_"),".csv"), 
                                      read.csv(file_list[i])) 

#import 2015 pop denom
pop_denom_100m<-read.csv("/Users/gretam/Documents/data/Lancet 2025/2015/2015_pop_denom_100m.csv")

#### Pop-weighted NDVI data mgmt (100m) ####
#combine all the seasons together in one long dataframe
seasons_popw_100m<-rbind(fall_num_100m, winter_num_100m, spring_num_100m, summer_num_100m)

#collapse to get mean and max of seasonal NDVI*pop numerator by city 
seasons_popw_100m<- seasons_popw_100m %>%
  group_by(ID_HDC_G0, city, country, UC_NM_MN, XC_ISO_LST) %>%
  summarize(annual_avg_num_2019_100m = mean(sum, na.rm = TRUE), 
            annual_max_num_2019_100m = max(sum, na.rm = TRUE) )

#merge in denominator, keeping just column to match on and sum of population 
seasons_popw_100m<-merge(seasons_popw_100m, pop_denom_100m[,c("ID_HDC_G0", "sum")], by="ID_HDC_G0")
names(seasons_popw_100m)[names(seasons_popw_100m) == "sum"] <- "Population_2015_100m"
seasons_popw_100m$PopWeight_Avg_NDVI_2019_100m<-seasons_popw_100m$annual_avg_num_2019_100m/seasons_popw_100m$Population_2015_100m
seasons_popw_100m$PopWeight_Peak_NDVI_2019_100m<-seasons_popw_100m$annual_max_num_2019_100m/seasons_popw_100m$Population_2015_100m

#get rid of a couple of unnecessary vars
seasons_popw_100m[c("annual_avg_num_2019", "annual_max_num_2019", "annual_avg_num_2019_100m", 
                    "annual_max_num_2019_100m")] <- list(NULL) 

write.csv(seasons_popw_100m, paste0(output_dir, 'data2019.csv'))