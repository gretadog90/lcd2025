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

#make boxplots for NDVI by region
a<-ggplot(hia, aes(x=sub.region, y=pct_diff, color=sub.region)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(position=position_jitter(0.2), size=.6)+
  xlab("")+ 
  ylab("Percent change in NDVI 2014-2018 v. 2019-2023")+
  ylim(-25, 30)+
  coord_flip()+
  theme(legend.position="none")
  
b<-ggplot(data=hia, aes(x=pct_diff, group=sub.region, fill=sub.region)) +
  geom_density(alpha=.4)+xlab("")+
  ylab("Density")+
  scale_x_continuous(breaks=c(-20,-10,0, 20, 20),
                     labels=c("-20%", "-10%", "0", "+10%", "+20%"))+
  theme(legend.position="none")

c<-boxplot2023<-ggplot(hia, aes(x=sub.region, y=delta_mortality, color=sub.region)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(position=position_jitter(0.2), size=.6)+
  xlab("")+ 
  ylab("    Associated changes in deaths per 100,000 from NDVI changes 2014-2018 v. 2019-2023")+
  ylim(-600, 600)+
  coord_flip()+
  theme(legend.position="none")
  
d<-ggplot(data=hia, aes(x=delta_mortality, group=sub.region, fill=sub.region)) +
  geom_density(alpha=.4)+xlab("")+
  ylab("Density")+labs(fill="Geographic region")+
  scale_x_continuous(breaks=c(-600,-300,0, 300, 600),
                     labels=c("+600 deaths", "+300 deaths", "0", "-300 deaths", "-600 deaths"))+
  theme(legend.position="none")


#set up the file to save figure
pdf(file = "graphs/subregion_panels.pdf")

figure <- ggarrange(a, b, c, d,
                    widths=c(8,8), heights=c(5,5),
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)

dev.off()