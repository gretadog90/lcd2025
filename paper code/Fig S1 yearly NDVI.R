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
data2014<-read.csv("output/data2014.csv")
data2014<-data2014[,c("ID_HDC_G0", "PopWeight_Avg_NDVI_2014_100m")]
data2016<-read.csv("output/data2016.csv")
data2016<-data2016[,c("ID_HDC_G0", "PopWeight_Avg_NDVI_2016_100m")]
data2017<-read.csv("output/data2017.csv")
data2017<-data2017[,c("ID_HDC_G0", "PopWeight_Avg_NDVI_2017_100m")]
data2018<-read.csv("output/data2018.csv")
data2018<-data2018[,c("ID_HDC_G0", "PopWeight_Avg_NDVI_2018_100m")]
data2019<-read.csv("output/data2019.csv")
data2019<-data2019[,c("ID_HDC_G0", "PopWeight_Avg_NDVI_2019_100m")]

#### merge all the yearly greenspace data sets together ####
lcd2025 <- lcd2025 %>%
  inner_join(data2014) %>% 
  inner_join(data2016) %>% 
  inner_join(data2017) %>%
  inner_join(data2018) %>%
  inner_join(data2019)

subset<-lcd2025[,c("city", "lc_group", "hdi_level", "clim_region",  "who_region", "sub.region",
                   "PopWeight_Avg_NDVI_2014_100m", "PopWeight_Avg_NDVI_2015_100m", 
                   "PopWeight_Avg_NDVI_2016_100m","PopWeight_Avg_NDVI_2017_100m", 
                   "PopWeight_Avg_NDVI_2018_100m","PopWeight_Avg_NDVI_2019_100m", 
                   "PopWeight_Avg_NDVI_2020_100m", "PopWeight_Avg_NDVI_2021_100m", 
                   "PopWeight_Avg_NDVI_2022_100m", "PopWeight_Avg_NDVI_2023_100m")]  

#reshape long so that each row represents a city/year combo               
long <- melt(setDT(subset), 
             id.vars = c("city", "lc_group", "hdi_level", "clim_region", "who_region", "sub.region"), 
             variable.name = "year")

#get rid of variable prefix/suffix so year now= "2015" etc.
long<-long %>% 
  mutate(year = str_remove(year, '^PopWeight_Avg_NDVI_'))
long<-long %>% 
  mutate(year = str_remove(year, '_100m$'))

## WHO_SUB_REGION
# create means by region/climate group
means_who_sub <- aggregate(long$value, by=list(long$sub.region, long$clim_region, long$year), 
                           function(x)mean(x, na.rm=TRUE))
# rename columns to match
colnames(means_who_sub) <- c("sub.region","clim_region","year", "mean")

#remove 20 from years so that it shows up better in graph
long$year<-gsub("^20","'",long$year)
means_who_sub$year<-gsub("^20","'",means_who_sub$year)

#plot
pdf(file = "graphs/line graph averages who subregion.pdf", width=13.5, height=8.5)
ggplot() +
  geom_line(data=long, aes(x=year, y=value, group=city, color=clim_region), alpha=.2, size=.5) +
  geom_line(data=means_who_sub, aes(x=year, y=mean,group=clim_region, color=clim_region), size=1)+
  scale_color_brewer(palette = "PuOr", name = "Köppen-Geiger\nclimate classification")+
  xlab('Year') + ylab('Population-weighted greenest season NDVI')+ facet_wrap(~sub.region, ncol=4)
dev.off()
