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

#remove one city (Kapoeta), for which shapefile is incorrect and there is no pop
lcd2025<- lcd2025[-which(lcd2025$city=='Kapoeta'),]

#remove 1km data
names(lcd2025)
data_100m<-select(lcd2025, c("Latitude", "Longitude", "ISO3", "city", "country", 
                             "lc_group", "who_region", "hdi_level", "clim_region",
                             "sub.region", ends_with("_100m"), 
                             starts_with("Avg_NDVI_"), starts_with("Peak_NDVI_"),
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

# create greenness indicator
global$greennessIndicator <- cut(global$PopWeightPeakNDVI, 
                   breaks=c(-Inf, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, Inf), 
                   labels=c('Exceptionally Low','Very Low', 'Low', 
                            'Moderate','High', 'Very High', 'Exceptionally High'))
                           
###### export data for GLOBAL tab ######
write.csv(global, 'global.csv')

<<<<<<< HEAD
lc_region <- aggregate(global, by=global$lc_group, 
                           function(x)mean(x, na.rm=TRUE))

=======
# summarize to lc_region level
lc_region<- global %>%
  group_by(lc_group, year) %>%
  summarize(
    AvgNDVI= mean(AvgNDVI),
    PeakNDVI= mean(PeakNDVI),
    PopWeightAvgNDVI = mean(PopWeightAvgNDVI),
    PopWeightPeakNDVI = mean(PopWeightPeakNDVI)
  )

#export data for lc_region tab
write.csv(lc_region, 'lc_region.csv')

# summarize to hdi level
hdi_group<- global %>%
  group_by(hdi_level, year) %>%
  summarize(
    AvgNDVI= mean(AvgNDVI),
    PeakNDVI= mean(PeakNDVI),
    PopWeightAvgNDVI = mean(PopWeightAvgNDVI),
    PopWeightPeakNDVI = mean(PopWeightPeakNDVI)
  )

#export data for hdi tab
write.csv(hdi_group, 'hdi_group.csv')

# summarize to who level
who_region<- global %>%
  group_by(who_region, year) %>%
  summarize(
    AvgNDVI= mean(AvgNDVI),
    PeakNDVI= mean(PeakNDVI),
    PopWeightAvgNDVI = mean(PopWeightAvgNDVI),
    PopWeightPeakNDVI = mean(PopWeightPeakNDVI)
  )

#export data for who tab
write.csv(who_region, 'who_region.csv')

# summarize to country level
country<- global %>%
  group_by(country, ISO3, year) %>%
  summarize(
    AvgNDVI= mean(AvgNDVI),
    PeakNDVI= mean(PeakNDVI),
    PopWeightAvgNDVI = mean(PopWeightAvgNDVI),
    PopWeightPeakNDVI = mean(PopWeightPeakNDVI)
  )

#export data for who tab
write.csv(country, 'country.csv')


# export pop for weighting var tab
#there's only pop data for 2015 and 2020
pop<- global[which(global$year==c("2015", "2020")),]

weighting_var<- pop %>%
  group_by(city, country, year) %>%
  summarize(
    Population= mean(Population)
  )

#export data for weighting var tab
write.csv(weighting_var, 'weighting_var.csv')
>>>>>>> d8ad4f6e5d479238db3c21f7039589e916d29616
