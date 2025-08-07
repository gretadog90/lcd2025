#########################################################################
# Line graph- 2015-2024 by HDI level
#########################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggmap)
library(maps)
library(tidytab)
library(ggpubr)
library(egg)
library(data.table)
library(stringr)
library(lemon)
library(gtable)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#merged exposure data
lcd2025<-read.csv("output/lcd2025.csv")
data2014<-read.csv("output/data2014.csv")
data2014<-data2014[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2014_100m")]
data2016<-read.csv("output/data2016.csv")
data2016<-data2016[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2016_100m")]
data2017<-read.csv("output/data2017.csv")
data2017<-data2017[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2017_100m")]
data2018<-read.csv("output/data2018.csv")
data2018<-data2018[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2018_100m")]
data2019<-read.csv("output/data2019.csv")
data2019<-data2019[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2019_100m")]

#### merge all the yearly greenspace data sets together ####
lcd2025 <- lcd2025 %>%
  inner_join(data2014) %>% 
  inner_join(data2016) %>% 
  inner_join(data2017) %>%
  inner_join(data2018) %>%
  inner_join(data2019)

subset<-lcd2025[,c("city", "lc_group", "hdi_level", "clim_region",  "who_region", "sub.region",
                   "PopWeight_Peak_NDVI_2014_100m", "PopWeight_Peak_NDVI_2015_100m", 
                   "PopWeight_Peak_NDVI_2016_100m","PopWeight_Peak_NDVI_2017_100m", 
                   "PopWeight_Peak_NDVI_2018_100m","PopWeight_Peak_NDVI_2019_100m", 
                   "PopWeight_Peak_NDVI_2020_100m", "PopWeight_Peak_NDVI_2021_100m", 
                   "PopWeight_Peak_NDVI_2022_100m", "PopWeight_Peak_NDVI_2023_100m")]  

#reshape long so that each row represents a city/year combo               
long <- melt(setDT(subset), 
             id.vars = c("city", "lc_group", "hdi_level", "clim_region", "who_region", "sub.region"), 
             variable.name = "year")

#get rid of variable prefix/suffix so year now= "2015" etc.
long<-long %>% 
  mutate(year = str_remove(year, '^PopWeight_Peak_NDVI_'))
long<-long %>% 
  mutate(year = str_remove(year, '_100m$'))

## WHO_SUB_REGION
# create means by region/climate group
means_who_region <- aggregate(long$value, by=list(long$sub.region,long$year), 
                           function(x)mean(x, na.rm=TRUE))
# rename columns to match
colnames(means_who_region) <- c("sub.region","year", "mean")

#remove 20 from years so that it shows up better in graph
means_who_region$year<-gsub("^20","'",means_who_region$year)

means_who_clim<- aggregate(long$value, by=list(long$clim_region,long$year), 
                              function(x)mean(x, na.rm=TRUE))
# rename columns to match
colnames(means_who_clim) <- c("clim_region","year", "mean")

#remove 20 from years so that it shows up better in graph
means_who_clim$year<-gsub("^20","'",means_who_clim$year)
means_who_clim<-means_who_clim[means_who_clim$clim_region!="Polar",]

## OVERALL TREND
# create means by region/climate group
overall <- aggregate(long$value, by=list(long$year), 
                              function(x)mean(x, na.rm=TRUE))
# rename columns to match
colnames(overall) <- c("year", "mean")
overall$year<-gsub("^20","'",overall$year)
overall$group<-"overall"

# LABELING CLIMATE
#get one label per line (default to last year)
labels <- means_who_clim %>%
  group_by(year) %>%
  filter(year == "'23")

#now manually move
labels$year[labels$clim_region=="Tropical"]<-"'14"
labels$mean[labels$clim_region=="Tropical"]<-0.342
labels$mean[labels$clim_region=="Continental"]<-0.336
labels$mean[labels$clim_region=="Temperate"]<-0.29
labels$year[labels$clim_region=="Arid"]<-"'22"
labels$mean[labels$clim_region=="Arid"]<-0.205

# LABELING REGIONS
#get one label per line (default to last year)
region_labels <- means_who_region %>%
  group_by(year) %>%
  filter(year == "'23")

#manually adjusting label positions because super crowded
print(region_labels$sub.region)

region_labels$year[region_labels$sub.region=="Melanesia"]<-"'21"
region_labels$mean[region_labels$sub.region=="Melanesia"]<-0.432

region_labels$year[region_labels$sub.region=="Northern America"]<-"'14"
region_labels$mean[region_labels$sub.region=="Northern America"]<-0.4

region_labels$mean[region_labels$sub.region=="Northern Europe"]<-0.37

region_labels$year[region_labels$sub.region=="Western Europe"]<-"'14"
region_labels$mean[region_labels$sub.region=="Western Europe"]<-0.37

region_labels$year[region_labels$sub.region=="Eastern Europe"]<-"'18"
region_labels$mean[region_labels$sub.region=="Eastern Europe"]<-0.365

region_labels$year[region_labels$sub.region=="Australia and New Zealand"]<-"'14"
region_labels$mean[region_labels$sub.region=="Australia and New Zealand"]<-0.32

region_labels$mean[region_labels$sub.region=="South-eastern Asia"]<-0.315

region_labels$year[region_labels$sub.region=="Southern Asia"]<-"'17"
region_labels$mean[region_labels$sub.region=="Southern Asia"]<-0.31

region_labels$mean[region_labels$sub.region=="Sub-Saharan Africa"]<-0.286

region_labels$year[region_labels$sub.region=="Southern Europe"]<-"'14"
region_labels$mean[region_labels$sub.region=="Southern Europe"]<-0.268

region_labels$year[region_labels$sub.region=="Central Asia"]<-"'19"
region_labels$mean[region_labels$sub.region=="Central Asia"]<-0.275
region_labels$year[region_labels$sub.region=="Latin America and the Caribbean"]<-"'23"
region_labels$mean[region_labels$sub.region=="Latin America and the Caribbean"]<-0.23
region_labels$year[region_labels$sub.region=="Eastern Asia"]<-"'14"
region_labels$mean[region_labels$sub.region=="Eastern Asia"]<-0.227
region_labels$mean[region_labels$sub.region=="Northern Africa"]<-0.185
region_labels$mean[region_labels$sub.region=="Western Asia"]<-0.16

#plot
a<-ggplot()+
  geom_line(data=means_who_region, aes(x=year, y=mean, group=sub.region, color=sub.region), size=.6) +
  geom_line(data=overall,aes(x=year, y=mean, group=group), color = "black", linetype = "dashed", size = .5)+
  xlab('Year') + ylab('Population-weighted greenest season NDVI')+
  ggrepel::geom_text_repel(size=3, data = region_labels, aes(x=year, y=mean, label = sub.region, color=sub.region)) +
  scale_y_continuous(limits = c(0.07, 0.47), breaks = seq(0.1, 0.4, by = 0.1))+
  theme(legend.position = "none")

b<-ggplot()+
  geom_line(data=means_who_clim, aes(x=year, y=mean,group=clim_region, color=clim_region), size=.6) +
  geom_line(data=overall,aes(x=year, y=mean, group=group), color = "black", linetype = "dashed", size = .5)+
  xlab('Year') + ylab('')+
  ggrepel::geom_text_repel(size=3, data = labels, aes(x=year, y=mean, label = clim_region, color=clim_region)) +
  scale_y_continuous(limits = c(0.07, 0.47), breaks = seq(0.1, 0.4, by = 0.1))+
  scale_color_brewer(palette = "PuOr")+
  theme(axis.ticks.y = element_blank(), legend.position = "none")

#set up the file to save figure
pdf(file ="graphs/Fig1 update1.pdf")

figure <- ggarrange(a, b,
                    widths=c(7,7), heights=11,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)

dev.off()


