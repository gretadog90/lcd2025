#########################################################################
# Figure 4: % Exposure by clim_region (diff and pct diff)
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
hia<-hia[!(hia$clim_region=="Polar"),]

hia$diff<-hia$ndvi2019_2023-hia$ndvi2014_2018
hist(hia$diff)
hia$pct_diff<-((hia$ndvi2019_2023-hia$ndvi2014_2018)/hia$ndvi2014_2018)*100
hist(hia$pct_diff)

hia<- hia %>%
  group_by(clim_region) %>% 
  mutate(order = (cur_group_id()-4)*-1)

#make boxplots for NDVI by region
a<-ggplot(hia, aes(x=reorder(clim_region, order), y=diff)) + 
  geom_boxplot(outlier.shape = NA,  lwd=.2)+
  geom_jitter(aes(color=clim_region), position=position_jitter(0.2), size=.8)+
  xlab("")+ 
  ylab("Absolute change in NDVI 2014-2018 v. 2019-2023")+
  scale_color_brewer(palette = "PuOr")+
  scale_y_continuous(breaks=c(-0.075, -.05,-.025, 0, .025, .05))+
  coord_flip()+
  theme(legend.position="none")

b<-ggplot(hia, aes(x=reorder(clim_region, order), y=pct_diff)) + 
  geom_boxplot(outlier.shape = NA,  lwd=.2)+
  geom_jitter(aes(color=clim_region), position=position_jitter(0.2), size=.8)+
  xlab("")+ 
  ylab("Percent change in NDVI 2014-2018 v. 2019-2023")+
  scale_y_continuous(breaks=c(-20,-10,0, 10, 20, 30),
                     labels=c("-20%", "-10%", "0", "+10%", "+20%", "+30%"))+
  scale_color_brewer(palette = "PuOr")+
  coord_flip()+
  theme(legend.position="none")

#set up the file to save figure
pdf(file = "graphs/Fig4 exposure by climate UPDATE.pdf")

figure <- ggarrange(a, b,
                    widths=8.5, heights=c(5,5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

dev.off()