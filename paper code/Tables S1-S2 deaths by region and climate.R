#########################################################################
# Table S7: HIA by region with error bars
#########################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(tidytab)
library(ggpubr)
library(egg)
library(ggridges)
library(dplyr)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#read in HIA results
hia<-read.csv("outputHIA/hia_100m_5yr.csv")
hia<-hia[!is.na(hia$ndvi2019_2023),]
hist(hia$delta_mortality)

hia_sum<- hia %>% 
  group_by(sub.region) %>% 
  summarize(
    delta_mortality = sum(delta_mortality),
    pop = sum(Population_2020_100m),
    lb=sum(lb),
    ub=sum(ub)
  )

hia_sum$delta_mortality_per100000<-(hia_sum$delta_mortality/hia_sum$pop)*100000
hia_sum$a<-hia_sum$lb
hia_sum$b<-hia_sum$ub
hia_sum <- transform(hia_sum, lb = pmin(a, b))
hia_sum <- transform(hia_sum, ub = pmax(a, b))

hia_sum$lb_per100000<-(hia_sum$lb/hia_sum$pop)*100000
hia_sum$ub_per100000<-(hia_sum$ub/hia_sum$pop)*100000

hia_sum<-subset(hia_sum, select=-c(a,b))
write.csv(hia_sum, "graphs/tableS2.csv")

climate_sum<-hia %>% 
  group_by(clim_region) %>% 
  summarize(
    delta_mortality = sum(delta_mortality),
    pop = sum(Population_2020_100m),
    lb=sum(lb),
    ub=sum(ub)
  )

climate_sum$delta_mortality_per100000<-(climate_sum$delta_mortality/climate_sum$pop)*100000
climate_sum$a<-climate_sum$lb
climate_sum$b<-climate_sum$ub
climate_sum <- transform(climate_sum, lb = pmin(a, b))
climate_sum <- transform(climate_sum, ub = pmax(a, b))

climate_sum$lb_per100000<-(climate_sum$lb/climate_sum$pop)*100000
climate_sum$ub_per100000<-(climate_sum$ub/climate_sum$pop)*100000

climate_sum<-subset(climate_sum, select=-c(a,b))
write.csv(climate_sum, "graphs/tableS3.csv")
