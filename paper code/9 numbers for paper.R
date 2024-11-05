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
un_countries = readxl::read_xlsx("groupings/UNSD — Methodology.xlsx")

#drop the one w/ misisng data and get diff
hia<-hia[!is.na(hia$ndvi2019_2023),]
hia$pct_diff<-((hia$ndvi2019_2023-hia$ndvi2014_2018)/hia$ndvi2014_2018)*100
hia$diff<-(hia$ndvi2019_2023-hia$ndvi2014_2018)

#countries not in the analysis
un_countries$`Country or Area`[!un_countries$`ISO-alpha3 Code` %in% lcd$ISO3]

#no. countries 
hia %>% summarise(count = n_distinct(country))

#get summary stats for FIG 1
summary(hia$PopWeight_Peak_NDVI_2014_100m)
summary(hia$PopWeight_Peak_NDVI_2015_100m)
summary(hia$PopWeight_Peak_NDVI_2016_100m)
summary(hia$PopWeight_Peak_NDVI_2017_100m)
summary(hia$PopWeight_Peak_NDVI_2018_100m)
summary(hia$PopWeight_Peak_NDVI_2019_100m)
summary(hia$PopWeight_Peak_NDVI_2020_100m)
summary(hia$PopWeight_Peak_NDVI_2021_100m)
summary(hia$PopWeight_Peak_NDVI_2022_100m)
summary(hia$PopWeight_Peak_NDVI_2023_100m)

city_range<-hia[,c("sub.region","clim_region", "city",
                   "PopWeight_Peak_NDVI_2014_100m", "PopWeight_Peak_NDVI_2015_100m",
                   "PopWeight_Peak_NDVI_2016_100m","PopWeight_Peak_NDVI_2017_100m", 
                   "PopWeight_Peak_NDVI_2018_100m","PopWeight_Peak_NDVI_2019_100m",
                   "PopWeight_Peak_NDVI_2020_100m", "PopWeight_Peak_NDVI_2021_100m",
                   "PopWeight_Peak_NDVI_2022_100m", "PopWeight_Peak_NDVI_2023_100m")]
                     
city_range$min<-as.numeric(apply(city_range, 1, FUN = min))
city_range$max<-as.numeric(apply(city_range[,4:13], 1, FUN = max))
city_range$range<-city_range$max-city_range$min
summary(city_range$range)
tapply(city_range$range, city_range$clim_region, summary)
tapply(city_range$range, city_range$sub.region, summary)

city_range$pctdiff_range<-(city_range$max-city_range$min)/city_range$min
summary(city_range$pctdiff_range)
city_range<- city_range[order(city_range$pctdiff_range),]
sum(city_range$pctdiff_range > .2)
  
#get summary stats for FIG 2
summary(hia$ndvi2014_2018)
summary(hia$ndvi2019_2023)
summary(hia$pct_diff)
tapply(hia$ndvi2014_2018, hia$sub.region, summary)
tapply(hia$ndvi2019_2023, hia$sub.region, summary)

#summary stats for FIG 3
#get regions from top and bottom most % change
tapply(hia$diff, hia$sub.region, summary)
tapply(hia$pct_diff, hia$sub.region, summary)

spread<-hia %>% 
  group_by(sub.region) %>% 
  summarise(
    maximum = max(diff),
    minimum = min(diff),
    max_pct = max(pct_diff),
    min_pct = min(pct_diff)
  )
spread$range<-spread$maximum-spread$minimum
spread$range_pct<-spread$max_pct-spread$min_pct

spread<- spread[order(spread$range),]
print(spread)

spread<- spread[order(spread$range_pct),]
print(spread)

hia<- hia[order(hia$pct_diff),]
hia$id<- seq.int(nrow(hia))
hia %>% 
  select(sub.region, pct_diff, id, hdi_level) %>% 
  head(., 25)

hia %>% 
  select(sub.region, pct_diff, id, hdi_level) %>% 
  tail(., 50)

#summary stats for FIG 4
tapply(hia$diff, hia$clim_region, summary)
tapply(hia$pct_diff, hia$clim_region, summary)

spread<-hia %>% 
  group_by(clim_region) %>% 
  summarise(
    maximum = max(diff),
    minimum = min(diff),
    max_pct = max(pct_diff),
    min_pct = min(pct_diff)
  )
spread$range<-spread$maximum-spread$minimum
spread$range_pct<-spread$max_pct-spread$min_pct

spread<- spread[order(spread$range),]
print(spread)

spread<- spread[order(spread$range_pct),]
print(spread)

hia<- hia[order(hia$pct_diff),]
hia %>% 
  select(clim_region, pct_diff, city) %>% 
  head(., 25)

hia<- hia[order(-hia$pct_diff),]
hia %>% 
  select(clim_region, pct_diff, city) %>% 
  head(., 25)

#summary stats for FIG 5
hia$delta_mortality<-(hia$delta_mortality/hia$Population_2020_100m)*100000
hia$ub<-(hia$ub/hia$Population_2020_100m)*100000
hia$lb<-(hia$lb/hia$Population_2020_100m)*100000
summary(hia$delta_mortality)
hist(hia$delta_mortality)
summary(hia$ub)
summary(hia$lb)
tapply(hia$delta_mortality, hia$clim_region, summary)
tapply(hia$delta_mortality, hia$sub.region, summary)

hia<- hia[order(hia$sub.region, hia$delta_mortality),]

#discussion
euro<-hia %>%
  mutate(Europe = ifelse(str_detect(sub.region, "Europe"),
                       1,
                       0))

tapply(euro$ndvi2019_2023, euro$Europe, summary)
tapply(euro$delta_mortality, euro$Europe, summary)
