#########################################################################
# Pct change by lc_group + hdi
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

#set up the file to save figure
pdf(file = "pct diff by lc region.pdf", width=8, height=5)

ggplot(lcd2025, aes(x=lc_group, y=pct_diff)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=lc_group), position=position_jitter(0.2), size=.6)+
  xlab("")+ 
  ylab("Percent change in NDVI 2015-2020 v. 2024")+
  scale_y_continuous(breaks=c(-50, -25,-10,0, 10, 25, 50),
                     labels=c("-50%", "-25%", "-10%", "0", "+10%", "+25%", "+50%"))+
  coord_flip()+
  theme(legend.position="none")

dev.off()

#make hdi into a factor var with a more sensibel order
lcd2025$hdi_level <- factor(lcd2025$hdi_level, levels = c("N/A", "Low", "Medium", "High" "Very High"))

#set up the file to save figure
pdf(file = "pct diff by hdi.pdf", width=8, height=5)

ggplot(lcd2025, aes(x=hdi_level, y=pct_diff)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color=hdi_level), position=position_jitter(0.2), size=.6)+
  scale_color_manual(values=c("Very High"="#63B8FF","High"="#D8BFD8","Medium"="#B8860B",
                              "Low"="#40E0D0", "N/A"="#708090"), name = "HDI level")+
  xlab("")+ 
  ylab("Percent change in NDVI 2015-2020 v. 2024")+
  scale_y_continuous(breaks=c(-50, -25,-10,0, 10, 25, 50),
                     labels=c("-50%", "-25%", "-10%", "0", "+10%", "+25%", "+50%"))+
  coord_flip()+
  theme(legend.position="none")

dev.off()