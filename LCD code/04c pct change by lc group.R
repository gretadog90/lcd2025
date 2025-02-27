#########################################################################
# Pct change by lc_group 
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
library(ggpubr)
library(egg)

####import####
setwd('~/Documents/data/Lancet 2025/LCD report')
import_dir='~/Documents/data/Lancet 2025/output/'

#import the merged dataset
lcd2025<-read.csv(paste0(import_dir, "lcd2025.csv"))
lcd2025<- lcd2025[-which(lcd2025$city=='Kapoeta'),]

#create a 2015-2020 baseline avg
lcd2025$baseline<-rowMeans(lcd2025[,c('PopWeight_Peak_NDVI_2015_100m', 'PopWeight_Peak_NDVI_2016_100m',
                                      'PopWeight_Peak_NDVI_2017_100m', 'PopWeight_Peak_NDVI_2018_100m',
                                      'PopWeight_Peak_NDVI_2019_100m', 'PopWeight_Peak_NDVI_2020_100m')])
#get pct_diff baseline v. 2024
lcd2025$pct_diff<-((lcd2025$PopWeight_Peak_NDVI_2024_100m-lcd2025$baseline)/lcd2025$baseline)*100

lcd2025 <- lcd2025 %>% 
  group_by(lc_group) %>% 
  mutate(subregion_mean = mean(pct_diff))

lcd2025<- lcd2025 %>%
  group_by(lc_group) %>% 
  mutate(order = row())


#set up the file to save figure
pdf(file = "pct diff by lc region")

a<-ggplot(lcd2025, aes(x=lc_group, y=pct_diff)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=lc_group), position=position_jitter(0.2), size=.6)+
  xlab("")+ 
  ylab("Percent change in NDVI 2015-2020 v. 2024")+
  scale_y_continuous(breaks=c(-20,-10,0, 10, 20, 30),
                     labels=c("-20%", "-10%", "0", "+10%", "+20%", "+30%"))+
  coord_flip()+
  theme(legend.position="none")


dev.off()