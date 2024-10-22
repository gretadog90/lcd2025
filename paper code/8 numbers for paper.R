#########################################################################
# Summary stats for paper
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
lcd<-read.csv("output/lcd2025.csv")

#get smumary stats for gba
lcd$Blue_Area_2020<- lcd$GreenBlue_Area_2020-lcd$Green_Area_2020
summary(lcd$Green_Area_2020)
summary(lcd$GreenBlue_Area_2020)
summary(lcd$Blue_Area_2020)

tapply(lcd$Green_Area_2020, lcd$sub.region, summary)
tapply(lcd$Blue_Area_2020, lcd$sub.region, summary)

tapply(lcd$Green_Area_2020, lcd$clim_region, summary)
tapply(lcd$Blue_Area_2020, lcd$clim_region, summary)


subregion<-read.csv("groupings/UNSD — Methodology.csv")
hia<-hia[!is.na(hia$ndvi2019_2023),]
hia$pct_diff<-((hia$ndvi2019_2023-hia$ndvi2014_2018)/hia$ndvi2014_2018)*100
hia$diff<-(hia$ndvi2019_2023-hia$ndvi2014_2018)

#get smumary stats for pop-weighted ndvi over time
summary(hia$ndvi2014_2018)
summary(hia$ndvi2019_2023)
tapply(hia$ndvi2019_2023, hia$sub.region, summary)
tapply(hia$ndvi2019_2023, hia$clim_region, summary)

summary(hia$pct_diff)
summary(hia$diff)

#get regions from top and bottom most % change
hia<- hia[order(hia$pct_diff),]
hia$id<- seq.int(nrow(hia))
hia %>% 
  select(sub.region, pct_diff, id, hdi_level) %>% 
  head(., 25)

hia %>% 
  select(sub.region, pct_diff, id, hdi_level) %>% 
  tail(., 50)

hia$delta_mortality<-(hia$delta_mortality/hia$Population_2020_100m)*100000
summary(hia$delta_mortality)
hist(hia$delta_mortality)

hia %>% summarise(count = n_distinct(country))