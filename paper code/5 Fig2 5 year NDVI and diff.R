#########################################################################
# Figure 1: Avg pop-weighted peak NDVI 2014-8, 2019-2023, %diff
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
hia<-read.csv("outputHIA/hia_100m_5yr.csv")
hia<-hia[!is.na(hia$ndvi2019_2023),]

hia$pct_diff<-((hia$ndvi2019_2023-hia$ndvi2014_2018)/hia$ndvi2014_2018)*100
hia$diff<-hia$ndvi2019_2023-hia$ndvi2014_2018

hist(hia$pct_diff)
hist(hia$diff)

#panel a: 2014-2018 100m pop weighted NDVI 
map_early<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white', size = .2) +
  geom_point(data = hia, aes(x = Longitude, y = Latitude, 
                                 colour = cut(ndvi2014_2018, c(0, .1, .2, .3, .4, .5, 1))), size=.5) +
  scale_color_manual(name = "2014-2018\nPopulation-weighted\nGreenest Season NDVI",
                     values=c("#EEB99F","#EAB64E","#E6E600","#63C600","#3b7600","#003100", "#F2F2F2"),
                     labels=c("0-0.09", "0.10-0.19", "0.20-0.29", "0.30-0.39", "0.40-0.49", "0.5+")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12))

#panel b: 2019-2023 100m pop weighted NDVI 
map_late<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white', size=.2) +
  geom_point(data = hia, aes(x = Longitude, y = Latitude, 
                             colour = cut(ndvi2019_2023, c(0, .1, .2, .3, .4, .5, 1))), size=.5) +
  scale_color_manual(name = "2019-2023\nPopulation-weighted\nGreenest Season NDVI",
                     values=c("#EEB99F","#EAB64E","#E6E600","#63C600","#3b7600","#003100", "#F2F2F2"),
                     labels=c("0-0.09", "0.10-0.19", "0.20-0.29", "0.30-0.39", "0.40-0.49", "0.5+")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12))

#panel c: 100m pop weighted NDVI pct diff
map_pctdiff<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white', size=.2) +
  geom_point(data = hia, aes(x = Longitude, y = Latitude, 
                                 colour =cut(pct_diff, c(-Inf, -20, -10, -5, 5, 10, 20, Inf))), size=.5) +
  scale_color_manual(name = "Percent Change", 
                     values=c("#B2182B", "#D6604D", "#F4A582", "#f2f2f2", "#92C5DE", "#4393C3", "#2166AC"),
                     labels=c(">20% decrease", "10-19% decrease", "5-9% decrease","<5% change", 
                              "5-9% increase", "10-19% increase", ">20% increase"))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12))

#set up the file to save figure
pdf(file = "graphs/Fig2 5yr UPDATE.pdf")

figure <- ggarrange(map_early, map_late, map_pctdiff,
                    widths =8, heights=c(5,5,5),
                    labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3)

dev.off()