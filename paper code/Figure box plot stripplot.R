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

#make boxplots for NDVI by region
boxplot2018<-ggplot(hia, aes(x=reorder(sub.region, order), y=ndvi2014_2018)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=clim_region), position=position_jitter(0.2), size=.6)+
  ylim(0, 0.6)+
  xlab("")+ 
  ylab("Average greenest-season NDVI (2014-2018)")+
  coord_flip()+
  theme(legend.position="none")
 
boxplot2023<-ggplot(hia, aes(x=reorder(sub.region, order), y=ndvi2019_2023)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=clim_region), position=position_jitter(0.2), size=.6)+
  xlab("")+ 
  ylab("Average greenest-season NDVI (2019-2023)")+
  ylim(0, 0.6)+
  coord_flip()


#set up the file to save figure
pdf(file = "graphs/boxplots.pdf")

figure <- ggarrange(boxplot2018, boxplot2023, 
                    widths =8, heights=c(5,5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

dev.off()

#greens=brewer.pal(5, name = "BrBG")
#greens <- colorRampPalette(greens)(15)

