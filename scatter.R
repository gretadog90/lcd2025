#########################################################################
# Scatter comparing 5 year averages
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
hia$diff<-hia$ndvi2019_2023-hia$ndvi2014_2018
summary(hia$diff)

#set up the file to save figure
pdf(file = "graphs/scatter who.pdf")

ggplot(hia, aes(x=ndvi2014_2018, y=ndvi2019_2023, color=who_region)) + 
  geom_point(size=1)+
  geom_abline(slope=1, intercept=0)

dev.off()

pdf(file = "graphs/scatter climate.pdf")
ggplot(hia, aes(x=ndvi2014_2018, y=ndvi2019_2023, color=clim_region)) + 
  geom_point(size=1) +
  geom_abline(slope=1, intercept=0)

dev.off()