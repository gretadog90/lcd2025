#########################################################################
# Figure S2. Map of the world with cities by climate classification. 
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
library(ggpubr)
library(egg)
library(readxl)
library(paletteer)
library(RColorBrewer)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#merged exposure data
hia<-read.csv("outputHIA/hia_100m_5yr.csv")
hia<-hia[!is.na(hia$ndvi2019_2023),]

pdf(file ="graphs/S2 climate class map city dots.pdf", height=4, width=9)

ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = hia, aes(x = Longitude, y = Latitude, 
                             colour =clim_region), size=.6) +
  scale_color_manual(name = "Köppen-Geiger\nclimate classification",
                     values=c("#E66101","#FDB863","#DEE6E7","#B2ABD2", "#5E3C99"))+
  theme_classic()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10), axis.line=element_blank())

dev.off()

