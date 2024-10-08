#########################################################################
# Figure 1: Using the indicator from LCD
#########################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(tidytab)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#merged exposure data
lcd2025<-read.csv("output/lcd2025.csv")
possible_values <- c("Exceptionally Low", "Very Low", "Low", "Moderate", "High", "Very High", "Exceptionally High", "NA")

tab(lcd2025$indicator_2015_100m)
hist(lcd2025$PopWeight_Peak_NDVI_2015_100m)

#panel a: 2015 100m pop weighted NDVI (indicator)
pdf(file = "graphs/Figure1_LCD_indicator.pdf")

## The map (maps + ggplot2 )
ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map,
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = lcd2025, aes(x = Longitude, 
                                 y = Latitude, color = indicator_2015_100m), size=1) +
  scale_color_manual(limits = possible_values, drop = FALSE, values=c("#EEB99F","#EAB64E","#E6E600","#63C600","#3b7600","#003100","#050B00", "#F2F2F2"), name = "2015 Urban Greenspace") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank())

dev.off()