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

data_100m<- data_100m %>% select(-contains("indicator"))
 
long <- data_100m %>%
  pivot_longer(
    cols = matches("[_20][1-2][0-9]$"), 
    names_to = "year",
    values_to = "value")      

long$year2 <- str_extract(long$year, "20[1-2][0-9]$")
long$year<-gsub('_20[1-2][0-9]', '', long$year)

test<- long %>%
  pivot_wider(names_from = year, values_from = year2)
test<-reshape(long, idvar = "year2", timevar = "year", direction = "wide")
