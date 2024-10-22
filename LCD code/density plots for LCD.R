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

ggplot(data=hia, aes(x=delta_mortality, group=sub.region, fill=sub.region)) +
  geom_density(alpha=.4)

ggplot(data=hia, aes(x=delta_mortality, group=who_region, fill=who_region)) +
  geom_density(alpha=.4)

ggplot(data=hia, aes(x=delta_mortality, group=hdi_level, fill=hdi_level)) +
  geom_density(alpha=.4)

ggplot(data=hia, aes(x=delta_mortality, group=lc_group, fill=lc_group)) +
  geom_density(alpha=.4)


dev.off()