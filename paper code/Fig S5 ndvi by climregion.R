#########################################################################
# Figure S5
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
  group_by(clim_region) %>% 
  mutate(ndvi_climregion= mean(ndvi2019_2023))

hia<- hia %>%
  group_by(ndvi_climregion) %>% 
  mutate(order = cur_group_id())

#figure out value for polar and then drop
polar<-subset(hia, clim_region=="Polar")
hia<-subset(hia, clim_region!="Polar")

#set up the file to save figure
pdf(file = "graphs/boxplots_climzone.pdf")

#make boxplots for NDVI by region
ggplot(hia, aes(x=reorder(clim_region, order), y=ndvi2019_2023)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=clim_region), position=position_jitter(0.2), size=.6)+
  scale_color_brewer(palette = "PuOr")+
  ylim(0, 0.6)+
  xlab("")+ 
  ylab("Average greenest-season NDVI (2019-2023)")+
  coord_flip()+
  theme(legend.position="none")

dev.off()
