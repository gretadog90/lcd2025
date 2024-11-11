#########################################################################
# Figure 5: HIA 2014-2018 v 2019-2023 by region and climate zone
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
library(ggridges)

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

#get count by clim_region
table(hia$clim_region)
table(hia$sub.region)

#add N to climate classifications for graph
hia$clim_region[hia$clim_region=="Arid"]<-"Arid (N=232)"
hia$clim_region[hia$clim_region=="Continental"]<-"Continental (N=143)"
hia$clim_region[hia$clim_region=="Temperate"]<-"Temperate (N=372)"
hia$clim_region[hia$clim_region=="Tropical"]<-"Tropical (N=293)"

hia$sub.region[hia$sub.region=="Australia and New Zealand"]<-"Australia and New Zealand (N=6)"
hia$sub.region[hia$sub.region=="Central Asia"]<-"Central Asia (N=10)"
hia$sub.region[hia$sub.region=="Eastern Asia"]<-"Eastern Asia (N=192)"
hia$sub.region[hia$sub.region=="Eastern Europe"]<-"Eastern Europe (N=51)"
hia$sub.region[hia$sub.region=="Latin America and the Caribbean"]<-"Latin America and the Caribbean (N=121)"

hia$sub.region[hia$sub.region=="Melanesia"]<-"Melanesia (N=3)"
hia$sub.region[hia$sub.region=="Northern Africa"]<-"Northern Africa (N=43)"
hia$sub.region[hia$sub.region=="Northern Europe"]<-"Northern Europe (N=19)"
hia$sub.region[hia$sub.region=="South-eastern Asia"]<-"South-eastern Asia (N=63)"
hia$sub.region[hia$sub.region=="Southern Asia"]<-"Southern Asia (N=248)"

hia$sub.region[hia$sub.region=="Southern Europe"]<-"Southern Europe (N=31)"
hia$sub.region[hia$sub.region=="Sub-Saharan Africa"]<-"Sub-Saharan Africa (N=111)"
hia$sub.region[hia$sub.region=="Western Asia"]<-"Western Asia (N=59)"
hia$sub.region[hia$sub.region=="Western Europe"]<-"Western Europe (N=27)"
hia$sub.region[hia$sub.region=="Northern America"]<-"Northern America (N=57)"

nopolar<-subset(hia, clim_region!="Polar")
a<-ggplot(data=hia, aes(x=delta_mortality, y=sub.region, fill=sub.region,)) +
  geom_density_ridges(stat="binline") +
  theme_ridges() + 
  xlab("")+
  ylab("")+
  labs(fill="Geographic Region")+
  scale_x_continuous(breaks=c(-500,-250,0, 250, 500),
                     labels=c("500 more", "250 more", "0", "250 fewer", "500 fewer"))+
  theme(legend.position = "none",
        axis.text=element_text(size=9),
        axis.title=element_text(size=11))

b<-ggplot(data=nopolar, aes(x=delta_mortality, y=clim_region, fill=clim_region,)) +
  geom_density_ridges(stat="binline") +
  theme_ridges() + 
  ylab("")+
  xlab("Attributable deaths per 100,000 from changes in NDVI")+
  labs(fill="Köppen-Geiger\nclimate classification")+
  scale_fill_brewer(palette = "PuOr")+
  scale_x_continuous(breaks=c(-500,-250,0, 250, 500),
                     labels=c("500 more", "250 more", "0", "250 fewer", "500 fewer"))+
  theme(legend.position = "none",
        axis.text=element_text(size=9),
        axis.title=element_text(size=11))

#set up the file to save figure
pdf(file ="graphs/Fig 5 mort by region and climate.pdf")

figure <- ggarrange(a, b,
                    widths=11, heights=c(5,5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

dev.off()