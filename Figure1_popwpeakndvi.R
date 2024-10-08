#########################################################################
# Figure 1: Pop-weighted peak NDVI 2015, 2020, 2023
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

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#merged exposure data
lcd2025<-read.csv("output/lcd2025.csv")

#see how these numbers are distributed
tab(lcd2025$indicator_2015_100m)
tab(lcd2025$indicator_2020_100m)
tab(lcd2025$indicator_2023_100m)
hist(lcd2025$PopWeight_Peak_NDVI_2015_100m)

#calculate % change vars for 2015-2020 and 2020-2023
lcd2025<-lcd2025%>%
  group_by(city)%>%
  mutate("pctDiff2020"=((PopWeight_Peak_NDVI_2020_100m-PopWeight_Peak_NDVI_2015_100m)/PopWeight_Peak_NDVI_2015_100m)*100, 
         "pctDiff2023"=((PopWeight_Peak_NDVI_2023_100m-PopWeight_Peak_NDVI_2020_100m)/PopWeight_Peak_NDVI_2020_100m)*100)

hist(lcd2025$pctDiff2020)   
hist(lcd2025$pctDiff2023)         

#panel a: 2015 100m pop weighted NDVI (indicator)
map_2015<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = lcd2025, aes(x = Longitude, y = Latitude, 
                                 colour = cut(PopWeight_Peak_NDVI_2015_100m, c(0, .1, .2, .3, .4, .5, 1))), size=1) +
  scale_color_manual(name = "2015 Population Weighted\nPeak Season NDVI",
                     values=c("#EEB99F","#EAB64E","#E6E600","#63C600","#3b7600","#003100", "#F2F2F2"),
                     labels=c("0-0.09", "0.10-0.19", "0.20-0.29", "0.30-0.39", "0.40-0.49", "0.5+")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

#panel b: 2020 100m pop weighted NDVI pct diff
map_2020<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = lcd2025, aes(x = Longitude, y = Latitude, 
                                 colour =cut(pctDiff2020, c(-Inf, -25, -15, -5, 5, 15, 25, Inf))), size=1) +
               scale_color_manual(name = "Percent Change 2015\nto 2020", 
                                  values=c("#B2182B", "#D6604D", "#F4A582", "#f2f2f2", "#92C5DE", "#4393C3", "#2166AC"),
                                  labels=c(">25% decrease", "15-24% decrease", "5-14% decrease","<5% change", 
                                           "5-14% increase", "15-24% increase", ">25% increase"))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

#panel c: 2023 100m pop weighted NDVI pct diff
map_2023<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = lcd2025, aes(x = Longitude, y = Latitude, 
                                 colour =cut(pctDiff2023, c(-Inf, -25, -15, -5, 5, 15, 25, Inf))), size=1) +
  scale_color_manual(name = "Percent Change 2020\nto 2023", 
                     values=c("#B2182B", "#D6604D", "#F4A582", "#f2f2f2", "#92C5DE", "#4393C3", "#2166AC"),
                     labels=c(">25% decrease", "15-24% decrease", "5-14% decrease","<5% change", 
                              "5-14% increase", "15-24% increase", ">25% increase"))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

#set up the file to save figure
pdf(file = "graphs/Figure1.pdf")

figure <- ggarrange(map_2015, map_2020, map_2023,
                    widths =8, heights=c(5,5,5),
                    labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3)

dev.off()