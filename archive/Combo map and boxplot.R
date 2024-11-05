#########################################################################
# Figure 1: NDVI by region and climate zone
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
library(RColorBrewer)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#read in HIA results
hia<-read.csv("outputHIA/hia_100m_5yr.csv")
hia<-hia[!is.na(hia$ndvi2019_2023),]

hia <- hia %>% 
  group_by(sub.region) %>% 
  mutate(ndvi_subregion_2014 = mean(ndvi2014_2018))

hia<- hia %>%
  group_by(ndvi_subregion_2014) %>% 
  mutate(order = cur_group_id())

#panel a: 2014-2018 100m pop weighted NDVI 
map_early<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = hia, aes(x = Longitude, y = Latitude, 
                             colour = cut(ndvi2014_2018, c(0, .1, .2, .3, .4, .5, 1))), size=.5) +
  scale_color_manual(name = "2014-2018\nPopulation-weighted\nGreenest Season NDVI",
                     values=c("#EEB99F","#EAB64E","#E6E600","#63C600","#3b7600","#003100", "#F2F2F2"),
                     labels=c("0-0.09", "0.10-0.19", "0.20-0.29", "0.30-0.39", "0.40-0.49", "0.5+")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

#panel b: 2019-2023 100m pop weighted NDVI 
map_late<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = hia, aes(x = Longitude, y = Latitude, 
                             colour = cut(ndvi2019_2023, c(0, .1, .2, .3, .4, .5, 1))), size=.5) +
  scale_color_manual(name = "2019-2023\nPopulation-weighted\nGreenest Season NDVI",
                     values=c("#EEB99F","#EAB64E","#E6E600","#63C600","#3b7600","#003100", "#F2F2F2"),
                     labels=c("0-0.09", "0.10-0.19", "0.20-0.29", "0.30-0.39", "0.40-0.49", "0.5+")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

#make boxplots for NDVI by region
boxplot2018<-ggplot(hia, aes(x=reorder(sub.region, order), y=ndvi2014_2018)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=sub.region), position=position_jitter(0.2), size=.6)+
  ylim(0, 0.6)+
  xlab("")+ 
  ylab("Average greenest-season NDVI (2014-2018)")+
  coord_flip()+
  theme(legend.position="none")

boxplot2023<-ggplot(hia, aes(x=reorder(sub.region, order), y=ndvi2019_2023)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=clim_region), position=position_jitter(0.2), size=.6)+
  scale_colour_discrete(name="Climate region")+
  xlab("")+ 
  ylab("Average greenest-season NDVI (2019-2023)")+
  ylim(0, 0.6)+
  coord_flip()

#set up the file to save figure
pdf(file = "graphs/maps with boxplots.pdf")

figure <- ggarrange(map_early, boxplot2018, map_late, boxplot2023, 
                    widths =10, heights=c(8,8,8,8),
                    labels = c("A", "B", "C", "D"),
                    ncol = 1, nrow = 4)

dev.off()




