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

hia$delta_mortality<-(hia$delta_mortality/hia$Population_2020_100m)*100000
summary(hia$delta_mortality)
hist(hia$delta_mortality)
