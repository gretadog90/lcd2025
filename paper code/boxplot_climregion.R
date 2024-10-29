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
  group_by(clim_region) %>% 
  mutate(ndvi_climregion= mean(ndvi2019_2023))

hia<- hia %>%
  group_by(ndvi_climregion) %>% 
  mutate(order = cur_group_id())

#set up the file to save figure
pdf(file = "graphs/boxplots_climzone.pdf")

#make boxplots for NDVI by region
ggplot(hia, aes(x=reorder(clim_region, order), y=ndvi2019_2023)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=sub.region), position=position_jitter(0.2), size=.6)+
  ylim(0, 0.6)+
  xlab("")+ 
  ylab("Average greenest-season NDVI (2019-2023)")+
  coord_flip()

dev.off()


boxplot2023<-ggplot(hia, aes(x=reorder(sub.region, order), y=ndvi2019_2023)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=clim_region), position=position_jitter(0.2), size=.6)+
  scale_colour_discrete(name="Climate region")+
  xlab("")+ 
  ylab("Average greenest-season NDVI (2019-2023)")+
  ylim(0, 0.6)+
  coord_flip()