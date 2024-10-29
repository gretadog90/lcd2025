#########################################################################
# Figure 3: NDVI diff and HIA 2014-2018 v 2019-2023 by climate region
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

#read in HIA results
hia<-read.csv("outputHIA/hia_100m_5yr.csv")
hia<-hia[!is.na(hia$ndvi2019_2023),]
hist(hia$delta_mortality)

hia$diff<-hia$ndvi2019_2023-hia$ndvi2014_2018
hist(hia$diff)

hia$delta_mortality<-(hia$delta_mortality/hia$Population_2020_100m)*100000
summary(hia$delta_mortality)
hist(hia$delta_mortality)

#drop polar b/c only one city
hia<-subset(hia, clim_region!="Polar")
polar<- subset(hia, clim_region=="Polar")
#get count by clim_region
table(hia$clim_region)

#add N to climate classifications for graph
hia$clim_region[hia$clim_region=="Arid"]<-"Arid (N=232)"
hia$clim_region[hia$clim_region=="Continental"]<-"Continental (N=143)"
hia$clim_region[hia$clim_region=="Temperate"]<-"Temperate (N=372)"
hia$clim_region[hia$clim_region=="Tropical"]<-"Tropical (N=293)"

a<-ggplot(hia, aes(x=clim_region, y=diff)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=clim_region), position=position_jitter(0.2), size=.6)+
  xlab("")+ 
  ylab("Change in NDVI 2014-2018 v. 2019-2023")+
  scale_color_brewer(palette = "PuOr")+
  scale_y_continuous(breaks=c(-0.075, -.05,-.025, 0, .025, .05))+
  coord_flip()+
  theme(legend.position="none")

b<-ggplot(data=hia, aes(x=delta_mortality, group=clim_region, fill=clim_region,)) +
  geom_density(alpha=.4)+xlab("Associated changes in deaths per 100,000")+
  ylab("Density")+
  labs(fill="Köppen-Geiger\nclimate classification")+
  scale_fill_brewer(palette = "PuOr")+
  scale_x_continuous(breaks=c(-500,-250,0, 250, 500),
                     labels=c("500 more", "250 more", "0", "250 fewer", "500 fewer"))

#set up the file to save figure
pdf(file ="graphs/clim region density HIA and diff.pdf")

figure <- ggarrange(a, b,
                    widths=8.5, heights=c(5,5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

dev.off()