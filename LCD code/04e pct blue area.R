#########################################################################
# Pct blue area
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
library(ggpubr)
library(egg)
library(data.table)
library(ggforce)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#merged exposure data
setwd('~/Documents/data/Lancet 2025/LCD report')
import_dir='~/Documents/data/Lancet 2025/output/'

#import the merged dataset
lcd2025<-read.csv(paste0(import_dir, "lcd2025.csv"))
lcd2025<- lcd2025[-which(lcd2025$city=='Kapoeta'),]

#generate an urban area var that will be 1-green or blue area
lcd2025$Blue_Area_2015<-lcd2025$GreenBlue_Area_2015-lcd2025$Green_Area_2015
lcd2025$Blue_Area_2020<-lcd2025$GreenBlue_Area_2020-lcd2025$Green_Area_2020
hist(lcd2025$Blue_Area_2020)
summary(lcd2025$Blue_Area_2020)
tapply(lcd2025$Blue_Area_2020, lcd2025$hdi_level, summary)

pdf(file = "pct blue area.pdf", width=8, height=5)

#map pct blue area
ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  ## third layer, pct blue area by city
  geom_point(data = lcd2025, aes(x = Longitude, y = Latitude, 
                                 colour = cut(Blue_Area_2020, 
                                              c(-Inf, 0, 0.1, 0.2, 0.3, Inf))), show.legend = TRUE, size=.5) +
  scale_color_manual(name = "Percent Blue Area",
                     values=c("grey","cadetblue2","deepskyblue2", "dodgerblue4", "darkblue"),
                     labels=c('No blue space','<10%','10-20%','20-30%', '>30%'),
                     drop=FALSE) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

dev.off()