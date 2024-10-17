#########################################################################
# Line graph
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

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#merged exposure data
lcd2025<-read.csv("output/lcd2025.csv")
lcd2025<-lcd2025[,c("ID_HDC_G0", "ISO3", "clim_region")]
who_region<-read.csv("groupings/who_regions.csv")
hia<-read.csv("outputHIA/hia_100m.csv")
data2017<-read.csv("output/data2017.csv")
data2017<-data2017[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2017_100m")]
data2018<-read.csv("output/data2018.csv")
data2018<-data2018[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2018_100m")]
data2022<-read.csv("output/data2022.csv")
data2022<-data2022[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2022_100m")]

#### merge all the yearly greenspace data sets together ####
lcd2025 <- merge(lcd2025, hia, by="ID_HDC_G0")
lcd2025 <- merge(lcd2025, data2017, by="ID_HDC_G0")
lcd2025 <- merge(lcd2025, data2018, by="ID_HDC_G0")
lcd2025 <- merge(lcd2025, data2022, by="ID_HDC_G0")

#find which names don't match and edit gbd names to match lcd universe
lcd2025$IS03[!lcd2025$IS03 %in% who_region$alpha.3]
lcd2025<-merge(lcd2025, who_region[,c("alpha.3", "sub.region")], 
               by.x="ISO3",by.y="alpha.3", all.x = TRUE)

lcd2025$e2020_all2015<-(lcd2025$e2020_all2015/lcd2025$Population_2015_100m)*100000
summary(lcd2025$e2020_all2015)

lcd2025_morethan10 <- lcd2025[abs(lcd2025$e2020_all2015) > 10,]

subset<-lcd2025_morethan10[,c("city", "lc_group", "hdi_level", "clim_region",  "sub.region",
                   "PopWeight_Peak_NDVI_2014_100m", "PopWeight_Peak_NDVI_2015_100m", 
                   "PopWeight_Peak_NDVI_2016_100m","PopWeight_Peak_NDVI_2017_100m", 
                   "PopWeight_Peak_NDVI_2018_100m","PopWeight_Peak_NDVI_2019_100m", 
                   "PopWeight_Peak_NDVI_2020_100m", "PopWeight_Peak_NDVI_2021_100m", 
                   "PopWeight_Peak_NDVI_2022_100m", "PopWeight_Peak_NDVI_2023_100m")]  

#reshape long so that each row represents a city/year combo               
long <- melt(setDT(subset), 
             id.vars = c("city", "lc_group", "hdi_level", "clim_region", "sub.region"), 
             variable.name = "year")

#get rid of variable prefix/suffix so year now= "2015" etc.
long<-long %>% 
  mutate(year = str_remove(year, '^PopWeight_Peak_NDVI_'))
long<-long %>% 
  mutate(year = str_remove(year, '_100m$'))

#set up the file to save figure
pdf(file = "graphs/line graph hia over 10.pdf", width=13.5, height=8.5)

## WHO_SUB_REGION
# create means by region/climate group
means_who_sub <- aggregate(long$value, by=list(long$sub.region, long$clim_region, long$year), 
                           function(x)mean(x, na.rm=TRUE))
# rename columns to match
colnames(means_who_sub) <- c("sub.region","clim_region","year", "value")
#plot
ggplot() +
  geom_line(data=long, aes(x=year, y=value, group=city, color=clim_region), alpha=.1) +
  geom_point(data=means_who_sub, aes(x=year, y=value, color=clim_region), size=1)+
  xlab('Year') + ylab('Pop-weighted peak season NDVI')+ facet_wrap(~sub.region, ncol=4)
dev.off()

#create a pct change over time var
lcd2025_pctChange <- subset[, c("city", "lc_group", "hdi_level", "clim_region",  "sub.region")]
lcd2025_pctChange$pctChange2014<-0
lcd2025_pctChange$pctChange2015<-(subset$PopWeight_Peak_NDVI_2015_100m-subset$PopWeight_Peak_NDVI_2014_100m)/subset$PopWeight_Peak_NDVI_2014_100m
lcd2025_pctChange$pctChange2016<-(subset$PopWeight_Peak_NDVI_2016_100m-subset$PopWeight_Peak_NDVI_2015_100m)/subset$PopWeight_Peak_NDVI_2015_100m
lcd2025_pctChange$pctChange2017<-(subset$PopWeight_Peak_NDVI_2017_100m-subset$PopWeight_Peak_NDVI_2016_100m)/subset$PopWeight_Peak_NDVI_2016_100m
lcd2025_pctChange$pctChange2018<-(subset$PopWeight_Peak_NDVI_2018_100m-subset$PopWeight_Peak_NDVI_2017_100m)/subset$PopWeight_Peak_NDVI_2017_100m
lcd2025_pctChange$pctChange2019<-(subset$PopWeight_Peak_NDVI_2019_100m-subset$PopWeight_Peak_NDVI_2018_100m)/subset$PopWeight_Peak_NDVI_2018_100m
lcd2025_pctChange$pctChange2020<-(subset$PopWeight_Peak_NDVI_2020_100m-subset$PopWeight_Peak_NDVI_2019_100m)/subset$PopWeight_Peak_NDVI_2019_100m
lcd2025_pctChange$pctChange2021<-(subset$PopWeight_Peak_NDVI_2021_100m-subset$PopWeight_Peak_NDVI_2020_100m)/subset$PopWeight_Peak_NDVI_2020_100m
lcd2025_pctChange$pctChange2022<-(subset$PopWeight_Peak_NDVI_2022_100m-subset$PopWeight_Peak_NDVI_2021_100m)/subset$PopWeight_Peak_NDVI_2021_100m
lcd2025_pctChange$pctChange2023<-(subset$PopWeight_Peak_NDVI_2023_100m-subset$PopWeight_Peak_NDVI_2022_100m)/subset$PopWeight_Peak_NDVI_2022_100m

#reshape long so that each row represents a city/year combo               
long_pctChange <- melt(setDT(lcd2025_pctChange), 
                       id.vars = c("city", "lc_group", "hdi_level", "clim_region", "sub.region"), 
                       variable.name = "year")

#get rid of variable prefix/suffix so year now= "2015" etc.
long_pctChange<-long_pctChange %>% 
  mutate(year = str_remove(year, '^pctChange'))
long_pctChange$value<-long_pctChange$value*100

#set up the file to save figure
pdf(file = "graphs/line graph pct change hia over 10.pdf", width=13.5, height=8.5)

## WHO_SUB_REGION
# create means by region/climate group
means_who_sub_pctChange <- aggregate(long_pctChange$value, 
                                     by=list(long_pctChange$sub.region, long_pctChange$clim_region, long_pctChange$year), 
                                     function(x)mean(x, na.rm=TRUE))
# rename columns to match
colnames(means_who_sub_pctChange) <- c("sub.region","clim_region","year", "value")
#plot
ggplot() +
  geom_line(data=long_pctChange, aes(x=year, y=value, group=city, color=clim_region), alpha=.1) +
  geom_point(data=means_who_sub_pctChange, aes(x=year, y=value, color=clim_region), size=1)+
  xlab('Year') + ylab('Pop-weighted peak season NDVI')+ facet_wrap(~sub.region, ncol=4)
dev.off()
