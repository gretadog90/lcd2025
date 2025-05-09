#########################################################################
# Figure S5: ABSOLUTE HIA 2014-2018 v 2019-2023 by region and climate zone
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

hia$delta_mortality<-hia$delta_mortality

summary(hia$delta_mortality)
hist(hia$delta_mortality)

#get count by clim_region
table(hia$clim_region)
table(hia$sub.region)

hia<- hia %>%
  group_by(sub.region) %>% 
  mutate(order = (cur_group_id()-15)*-1)

nopolar<-subset(hia, clim_region!="Polar")

nopolar<- nopolar %>%
  group_by(clim_region) %>% 
  mutate(order = (cur_group_id()-4)*-1)

a<-ggplot(data=hia, aes(x=delta_mortality, y=reorder(sub.region, order), fill=sub.region,)) +
  geom_violin(width=2.1, size=0.2) +
  xlab("")+
  ylab("")+
  labs(fill="Geographic Region")+
  scale_x_continuous(breaks=c(-2000,-1000, 0,  1000),
                     labels=c("2,000 fewer", "1,000 fewer", "0",  "1,000 more"))+
  theme(legend.position = "none",
        axis.text=element_text(size=11),
        axis.title=element_text(size=13))

b<-ggplot(data=nopolar, aes(x=delta_mortality, y=reorder(clim_region, order), fill=clim_region,)) +
  geom_violin(width=2.1, size=0.2) +
  ylab("")+
  xlab("Attributable deaths from changes in NDVI")+
  labs(fill="Köppen-Geiger\nclimate classification")+
  scale_fill_brewer(palette = "PuOr")+
  scale_x_continuous(breaks=c(-2000,-1000, 0, 1000),
                     labels=c("2,000 fewer", "1,000 fewer", "0", "1,000 more"))+
  theme(legend.position = "none",
        axis.text=element_text(size=11),
        axis.title=element_text(size=13))

#set up the file to save figure
pdf(file ="graphs/FigS6 abs mort by region and climate.pdf")

figure <- ggarrange(a, b,
                    widths=11, heights=c(5,5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

dev.off()