#########################################################################
# Multipanel: Greenness Indicator + %diff 2024 v. 2015-2020 pop-weighted peak NDVI
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

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/LCD report')
import_dir='~/Documents/data/Lancet 2025/output/'

#import the merged dataset
lcd2025<-read.csv(paste0(import_dir, "lcd2025.csv"))

#remove one city (Kapoeta), for which shapefile is incorrect and there is no pop
lcd2025<- lcd2025[-which(lcd2025$city=='Kapoeta'),]

#create a 2015-2020 baseline avg
lcd2025$baseline<-rowMeans(lcd2025[,c('PopWeight_Peak_NDVI_2015_100m', 'PopWeight_Peak_NDVI_2016_100m',
                                      'PopWeight_Peak_NDVI_2017_100m', 'PopWeight_Peak_NDVI_2018_100m',
                                      'PopWeight_Peak_NDVI_2019_100m', 'PopWeight_Peak_NDVI_2020_100m')])
#get pct_diff baseline v. 2024
lcd2025$pct_diff<-((lcd2025$PopWeight_Peak_NDVI_2024_100m-lcd2025$baseline)/lcd2025$baseline)*100
hist(lcd2025$pct_diff)

#panel a: 2024 greenness indicator
map_2024<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = lcd2025, aes(x = Longitude, y = Latitude, 
                                 colour = cut(PopWeight_Peak_NDVI_2024_100m, 
                                              c(-Inf, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, Inf))), show.legend = TRUE, size=.5) +
  scale_color_manual(name = "Level of Urban Greenness",
                     values=c("#EEB99F","#EAB64E","#E6E600","#63C600","#3b7600","#003100", "#F2F2F2"),
                     labels=c('Exceptionally Low (0-0.19)','Very Low (0.20-0.29)', 'Low (0.30-0.39)', 
                              'Moderate (0.40-0.49)','High (0.50-0.59)', 'Very High (0.60-0.69)', 'Exceptionally High (>0.70)'),
                     drop=FALSE) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8))

#panel b: pct diff 2015-2020 v 2024 pop weighted peak NDVI 
map_pctdiff<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = lcd2025, aes(x = Longitude, y = Latitude, 
                                 colour =cut(pct_diff, c(-Inf, -20, -10, -5, 5, 10, 20, Inf))), size=.5) +
  scale_color_manual(name = "Percent Change \n2015-2020 v 2024", 
                     values=c("#2166AC","#4393C3", "#92C5DE", "#f2f2f2","#F4A582","#D6604D","#B2182B"),
                     labels=c(">20% decrease", "10-19% decrease", "5-9% decrease","<5% change", 
                              "5-9% increase", "10-19% increase", ">20% increase"))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8))

#set up the file to save figure
pdf(file = "main fig.pdf")

figure <- ggarrange(map_2024, map_pctdiff,
                    widths =11, heights=c(5,5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

dev.off()