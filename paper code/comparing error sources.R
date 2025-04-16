#########################################################################
# fraction of error from HR v baseline mort
#########################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(tidytab)
library(ggpubr)
library(egg)
library(ggridges)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#read in HIA results
hia<-read.csv("outputHIA/hia_100m_5yr.csv")
hia<-hia[!is.na(hia$ndvi2019_2023),]
hist(hia$delta_mortality)

hia$a<-hia$lb
hia$b<-hia$ub
hia <- transform(hia, lb = pmin(a, b))
hia<- transform(hia, ub = pmax(a, b))
hia$diff_bounds<-hia$ub-hia$delta_mortality
hia$diff_mort<-hia$upper.2020-hia$val.2020
hia$diff_hr<-hia$paf_ub-hia$paf

summary(hia$diff_mort)
summary(hia$diff_hr)
