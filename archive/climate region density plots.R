#########################################################################
# Figure 3: % change NDVI and HIA 2014-2018 v 2019-2023 by climate region
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

hia$pct_diff<-((hia$ndvi2019_2023-hia$ndvi2014_2018)/hia$ndvi2014_2018)*100
hist(hia$pct_diff)

hia$delta_mortality<-(hia$delta_mortality/hia$Population_2020_100m)*100000
summary(hia$delta_mortality)
hist(hia$delta_mortality)

#drop polar b/c only one city
hia<-subset(hia, clim_region!="Polar")

a<-ggplot(data=hia, aes(x=pct_diff, group=clim_region, fill=clim_region,)) +
  geom_density(alpha=.4)+xlab("Percent change in NDVI 2014-2018 v. 2019-2023")+
  ylab("Density")+
  scale_x_continuous(breaks=c(-20,-10,0, 10, 20, 30),
                     labels=c("-20%", "-10%", "0", "+10%", "+20%", "+30%"))+
  theme(legend.position="none")

b<-ggplot(data=hia, aes(x=delta_mortality, group=clim_region, fill=clim_region,)) +
  geom_density(alpha=.4)+xlab("Associated changes in deaths per 100,000")+
  ylab("Density")+labs(fill="Köppen-Geiger\nclimate classification")+
  scale_x_continuous(breaks=c(-500,-250,0, 250, 500),
                     labels=c("500 more", "250 more", "0", "250 fewer", "500 fewer"))

#set up the file to save figure
pdf(file ="graphs/clim region density HIA and pct diff.pdf")

figure <- ggarrange(a, b,
                    widths=8.5, heights=c(5,5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

dev.off()
