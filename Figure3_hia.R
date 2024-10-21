#########################################################################
# Figure 3: HIA 2014-2018 v 2019-2023 
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
hia<-hia[!is.na(hia$ndvi2019_2023),]
hist(hia$delta_mortality)

hia$delta_mortality<-(hia$delta_mortality/hia$Population_2020_100m)*100000
summary(hia$delta_mortality)
hist(hia$delta_mortality)

#set up the file to save figure
pdf(file ="graphs/HIA 5yr.pdf", width =10, height=5)

#panel a: 2015-2020 HIA results (2015 pop/mort)
ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = hia, aes(x = Longitude, y = Latitude, 
                             colour=cut(delta_mortality, c(-Inf, -250, -100, -50, -25, -5, 5, 25, 50, 100, 250, Inf))), size=.5) +
  scale_color_manual(name = "Change in deaths per 100,000\nto 2020 population from\nNDVI differences\n2014-18 to 2019-2023", 
                     values=c( "#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE",
                               "#4393C3", "#2166AC", "#053061"),
                     labels=c("250+ fewer", "100+ fewer", "50+ fewer", "25+ fewer", "5+ fewer", "less than 5 change", 
                              "5+ more", "25+ more", "50+ more", "100+ more", "250+ more"))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

dev.off()
