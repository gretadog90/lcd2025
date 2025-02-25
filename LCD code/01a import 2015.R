################################################################
# Import 2015 GEE data to create the yearly NDVI metrics
################################################################

#### Set up ####
#clear objects
rm(list = ls()) 

#load in relevant libraries
library(tidyr)
library(tidyverse)
library(dplyr)

#set working directory
setwd('~/Documents/data/Lancet 2025/2015')
output_dir="/Users/gretamartin/Documents/data/Lancet 2025/output/"

####import####
#get list of all files ending in .csv in wd
file_list = list.files(pattern="\\.csv$") 

#for each file in file list, rename to drop "2015_" and ".csv" and read in csv as object
for (i in 1:length(file_list)) assign(str_remove(str_remove(file_list[i], "2015_"),".csv"), 
                                      read.csv(file_list[i])) 

#### Seasonal NDVI data mgmt ####
#combine all the seasons together in one long dataframe
seasons<-rbind(fall,winter,spring,summer)

#collapse to get mean and max of season mean NDVI by city
seasons<- seasons %>%
  group_by(ID_HDC_G0, city, country, UC_NM_MN, XC_ISO_LST) %>%
  summarize(Avg_NDVI_2015 = mean(mean, na.rm = TRUE), Peak_NDVI_2015=max(mean, na.rm = TRUE))

#### Pop-weighted NDVI data mgmt (1km) ####
#combine all the seasons together in one long dataframe
seasons_popw<-rbind(fall_num, winter_num, spring_num, summer_num)

#collapse to get mean and max of seasonal NDVI*pop numerator by city 
seasons_popw<- seasons_popw %>%
  group_by(ID_HDC_G0, city, country, UC_NM_MN, XC_ISO_LST) %>%
  summarize(annual_avg_num_2015 = mean(sum, na.rm = TRUE), annual_max_num_2015 = max(sum, na.rm = TRUE) )

#merge in denominator, keeping just column to match on and sum of population 
seasons_popw<-merge(seasons_popw, pop_denom[,c("ID_HDC_G0", "sum")], by="ID_HDC_G0")
names(seasons_popw)[names(seasons_popw) == "sum"] <- "Population_2015"
seasons_popw$PopWeight_Avg_NDVI_2015<-seasons_popw$annual_avg_num_2015/seasons_popw$Population_2015
seasons_popw$PopWeight_Peak_NDVI_2015<-seasons_popw$annual_max_num_2015/seasons_popw$Population_2015

#### Pop-weighted NDVI data mgmt (100m) ####
#combine all the seasons together in one long dataframe
seasons_popw_100m<-rbind(fall_num_100m, winter_num_100m, spring_num_100m, summer_num_100m)

#collapse to get mean and max of seasonal NDVI*pop numerator by city 
seasons_popw_100m<- seasons_popw_100m %>%
  group_by(ID_HDC_G0, city, country, UC_NM_MN, XC_ISO_LST) %>%
  summarize(annual_avg_num_2015_100m = mean(sum, na.rm = TRUE), 
            annual_max_num_2015_100m = max(sum, na.rm = TRUE) )

#merge in denominator, keeping just column to match on and sum of population 
seasons_popw_100m<-merge(seasons_popw_100m, pop_denom_100m[,c("ID_HDC_G0", "sum")], by="ID_HDC_G0")
names(seasons_popw_100m)[names(seasons_popw_100m) == "sum"] <- "Population_2015_100m"
seasons_popw_100m$PopWeight_Avg_NDVI_2015_100m<-seasons_popw_100m$annual_avg_num_2015_100m/seasons_popw_100m$Population_2015_100m
seasons_popw_100m$PopWeight_Peak_NDVI_2015_100m<-seasons_popw_100m$annual_max_num_2015_100m/seasons_popw_100m$Population_2015_100m

#### Green and blue area ####
#create new data frame to hold ga and gba
landcover <- data.frame(ID_HDC_G0 = ga$ID_HDC_G0, Green_Area_2015 = ga$mean,
                        GreenBlue_Area_2015=gba$mean
)

#### Combine all 2015 measures together based measures ####
data2015<-seasons %>%
  inner_join(seasons_popw) %>% 
  inner_join(seasons_popw_100m) %>%
  inner_join(landcover)
  
#get rid of a couple of unnecessary vars
data2015[c("annual_avg_num_2015", "annual_max_num_2015", "annual_avg_num_2015_100m", 
           "annual_max_num_2015_100m")] <- list(NULL) 

write.csv(data2015, paste0(output_dir, 'data2015.csv'))
