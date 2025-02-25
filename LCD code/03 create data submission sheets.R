#########################################################################
# Create data sheets for LCD submission
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
setwd('~/Documents/data/Lancet 2025/LCD report')
import_dir='~/Documents/data/Lancet 2025/output/'

#import the merged dataset
lcd2025<-read.csv(paste0(import_dir, "lcd2025.csv"))

#remove 1km data
names(lcd2025)
data_100m<-select(lcd2025, c("Latitude", "Longitude", "ISO3", "city", "country", 
                             "lc_group", "who_region", "hdi_level", "clim_region",
                             "sub.region", ends_with("_100m"), 
                             "Green_Area_2015", "GreenBlue_Area_2015",
                             "Green_Area_2020", "GreenBlue_Area_2020"))
names(data_100m) <- sub("_100m", "", names(data_100m))

#get rid of indicator variable because R can't reshape both numeric & cat
data_100m<- data_100m %>% select(-contains("indicator"))

#remove all underscores but last (so that we can seperate off year)
data_100m<- data_100m %>% setNames(gsub("(_[^_]*)$|_", "\\1", names(.)))

#reshape long-- each row now has city, year, and indicator columns
long <- data_100m %>%
  pivot_longer(
    cols = matches("[_20][1-2][0-9]$"), 
    names_to = c("indicator", "year"),
    names_sep = "_")  

#reshape wide-- now each row has city, year columns plus one column for each 
# indicator i.e. peakNDVI, avgNDVI 
global <- long %>%
  pivot_wider(names_from = indicator, values_from = value)

#export data for global tab
write.csv(data2015, paste0(output_dir, 'data2015.csv'))
