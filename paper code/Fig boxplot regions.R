#########################################################################
# Figure 2: % Diff by sub.region and HIA per 100,000 by sub.region
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

hia$pct_diff<-((hia$ndvi2019_2023-hia$ndvi2014_2018)/hia$ndvi2014_2018)*100
hist(hia$pct_diff)

hia$delta_mortality<-(hia$delta_mortality/hia$Population_2020_100m)*100000
summary(hia$delta_mortality)
hist(hia$delta_mortality)

hia <- hia %>% 
  group_by(sub.region) %>% 
  mutate(subregion_mean = mean(pct_diff))

hia<- hia %>%
  group_by(subregion_mean) %>% 
  mutate(order = cur_group_id())

#make boxplots for NDVI by region
a<-ggplot(hia, aes(x=reorder(sub.region, order), y=pct_diff)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=sub.region), position=position_jitter(0.2), size=.6)+
  xlab("")+ 
  ylab("Percent change in NDVI 2014-2018 v. 2019-2023")+
  scale_y_continuous(breaks=c(-20,-10,0, 10, 20, 30),
                     labels=c("-20%", "-10%", "0", "+10%", "+20%", "+30%"))+
  coord_flip()+
  theme(legend.position="none")

b<-ggplot(hia, aes(x=reorder(sub.region, order), y=delta_mortality)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=sub.region), position=position_jitter(0.2), size=.6)+
  xlab("")+ 
  ylab("Associated changes in deaths per 100,000")+
  scale_y_continuous(breaks=c(-500,-250,0, 250, 500),
                     labels=c("500 more", "250 more", "0", "250 fewer", "500 fewer"))+
  coord_flip()+
  theme(legend.position="none")

#set up the file to save figure
pdf(file = "graphs/subregion_boxplots.pdf")

figure <- ggarrange(a, b,
                    widths=8.5, heights=c(5,5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

dev.off()