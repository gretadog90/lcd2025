#########################################################################
# Figure 5 UPDATE: HIA 2014-2018 v 2019-2023 by region and climate zone
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

hia$diff<-hia$ndvi2019_2023-hia$ndvi2014_2018
#hist(hia$diff)

hia$delta_mortality_std<-(hia$delta_mortality/hia$Population_2020_100m)*100000
summary(hia$delta_mortality_std)
#hist(hia$delta_mortality_std)

hia<- hia %>%
  group_by(sub.region) %>% 
  mutate(avg_lat = mean(Latitude))

nopolar<-subset(hia, clim_region!="Polar")

nopolar<- nopolar %>%
  group_by(clim_region) %>% 
  mutate(order = (cur_group_id()-4)*-1)

a<-ggplot(data=hia, aes(x=delta_mortality_std, y=reorder(sub.region, avg_lat), fill=sub.region,)) +
  geom_violin(width=2.1, size=0.2) +
  xlab("Attributable deaths per 100,000\n from changes in NDVI")+
  ylab("")+
  labs(fill="Geographic Region")+
  scale_x_continuous(breaks=c(-25,-10,0, 10, 25),
                     labels=c("25 fewer", "10 fewer", "0", "10 more", "25 more"))+
  theme(legend.position = "none",
        axis.text=element_text(size=8),
        axis.title=element_text(size=9))

c<-ggplot(data=nopolar, aes(x=delta_mortality_std, y=reorder(clim_region, order), fill=clim_region,)) +
  geom_violin(width=2.1, size=0.2) +
  ylab("")+
  xlab("Attributable deaths per 100,000\n from changes in NDVI")+
  scale_fill_brewer(palette = "PuOr")+
  scale_x_continuous(breaks=c(-25,-10,0, 10, 25),
                     labels=c("25 fewer", "10 fewer", "0", "10 more", "25 more"))+
  theme(legend.position = "none",
        axis.text=element_text(size=8),
        axis.title=element_text(size=9),)

b<-ggplot(data=hia, aes(x=delta_mortality, y=reorder(sub.region, avg_lat), fill=sub.region,)) +
  geom_violin(width=2.1, size=0.2) +
  xlab("Attributable deaths from changes in NDVI")+
  ylab("")+
  scale_x_continuous(breaks=c(-2000,-1000, 0,  1000),
                     labels=c("2,000 fewer", "1,000 fewer", "0",  "1,000 more"))+
  theme(legend.position = "none",
        axis.text.x=element_text(size=8),
        axis.title.x=element_text(size=9),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank())

d<-ggplot(data=nopolar, aes(x=delta_mortality, y=reorder(clim_region, order), fill=clim_region,)) +
  geom_violin(width=2.1, size=0.2) +
  ylab("")+
  xlab("Attributable deaths from changes in NDVI")+
  scale_fill_brewer(palette = "PuOr")+
  scale_x_continuous(breaks=c(-2000,-1000, 0, 1000),
                     labels=c("2,000 fewer", "1,000 fewer", "0", "1,000 more"))+
  theme(legend.position = "none",
        axis.text.x=element_text(size=8),
        axis.title.x=element_text(size=9),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank())

#set up the file to save figure
pdf(file ="graphs/Fig5 UPDATE mort by region and climate violin.pdf")

figure <- ggarrange(a, b, c, d,
                    widths=c(7, 9), heights=c(4,4),
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)

dev.off()

