#########################################################################
# Figure S4
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
  mutate(ndvi_subregion_2019 = mean(ndvi2019_2023))

hia<- hia %>%
  group_by(ndvi_subregion_2019) %>% 
  mutate(order = cur_group_id())

#set up the file to save figure
pdf(file = "graphs/boxplot_region.pdf")

#make boxplots for NDVI by region
ggplot(hia, aes(x=reorder(sub.region, order), y=ndvi2019_2023)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=sub.region), position=position_jitter(0.2), size=.6)+
  ylim(0, 0.6)+
  xlab("")+ 
  ylab("Average greenest-season NDVI (2019-2023)")+
  coord_flip()+
  theme(legend.position="none")

dev.off()
