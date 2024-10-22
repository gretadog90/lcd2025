#########################################################################
# Figure 3: HIA 2014-2018 v 2019-2023 
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

hia$delta_mortality<-(hia$delta_mortality/hia$Population_2020_100m)*100000
summary(hia$delta_mortality)
hist(hia$delta_mortality)

#drop polar b/c only one city
hia<-subset(hia, clim_region!="Polar")

#set up the file to save figure
pdf(file ="graphs/clim region density HIA.pdf", width =8, height=5)

ggplot(data=hia, aes(x=delta_mortality, group=clim_region, fill=clim_region,)) +
  geom_density(alpha=.4)+xlab("Associated changes in deaths per 100,000\nfrom NDVI changes 2014-2018 v. 2019-2023")+
  ylab("Density")+labs(fill="Köppen-Geiger\nclimate classification")+
  scale_x_continuous(breaks=c(-600,-300,0, 300, 600),
                 labels=c("600 excess deaths", "300 excess deaths", "0", "300 averted deaths", "600 averted deaths"))

dev.off()

