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

#set up the file to save figure
pdf(file ="graphs/regional density HIA.pdf", width =8, height=5)

ggplot(data=hia, aes(x=delta_mortality, group=clim_region, fill=clim_region,)) +
  geom_density(alpha=.4)+xlab("Change in mortality 2014-2018 v. 2019-2023")+
  ylab("Density")+labs(fill="Köppen-Geiger\nclimate classification")

dev.off()

