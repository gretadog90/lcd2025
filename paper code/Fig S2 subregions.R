#########################################################################
# Figure S2. Map of the world by geographic region classification. 
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
library(readxl)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#merged exposure data
subgroups<-read_excel("groupings/UNSD — Methodology.xlsx")

#map of world
world_map = map_data('world')

#see how aligned the country names are
subgroups$`Country or Area`[!subgroups$`Country or Area` %in% world_map$region]

#53 that don't match. many are not countries but territories or not in our analysis
subgroups$country<-subgroups$`Country or Area`
subgroups$sub.region<-subgroups$`Sub-region Name`
subgroups$country[subgroups$country=="United Republic of Tanzania"]<-"Tanzania"
subgroups$country[subgroups$country=="Congo"]<-"Republic of Congo"
subgroups$country[subgroups$country=="Eswatini"]<-"Swaziland"
subgroups$country[subgroups$country=="Côte d’Ivoire"]<-"Ivory Coast"
subgroups$country[subgroups$country=="Trinidad and Tobago"]<-"Trinidad"

subgroups$country[subgroups$country=="Bolivia (Plurinational State of)"]<-"Bolivia"
subgroups$country[subgroups$country=="Venezuela (Bolivarian Republic of)"]<-"Venezuela"
subgroups$country[subgroups$country=="United States of America"]<-"USA"
subgroups$country[subgroups$country=="China, Hong Kong Special Administrative Region"]<-"China"
subgroups$country[subgroups$country=="China, Macao Special Administrative Region"]<-"China"

subgroups$country[subgroups$country=="Democratic People's Republic of Korea"]<-"North Korea"
subgroups$country[subgroups$country=="Republic of Korea"]<-"South Korea"
subgroups$country[subgroups$country=="Lao People's Democratic Republic"]<-"Laos"
subgroups$country[subgroups$country=="Brunei Darussalam"]<-"Brunei"
subgroups$country[subgroups$country=="Viet Nam"]<-"Vietnam"

subgroups$country[subgroups$country=="Iran (Islamic Republic of)"]<-"Iran"
subgroups$country[subgroups$country=="State of Palestine"]<-"Palestine"
subgroups$country[subgroups$country=="Syrian Arab Republic"]<-"Syria"
subgroups$country[subgroups$country=="Türkiye"]<-"Turkey"
subgroups$country[subgroups$country=="Czechia"]<-"Czech Republic"

subgroups$country[subgroups$country=="Republic of Moldova"]<-"Moldova"
subgroups$country[subgroups$country=="Russian Federation"]<-"Russia"
subgroups$country[subgroups$country=="United Kingdom of Great Britain and Northern Ireland"]<-"UK"
subgroups$country[subgroups$country=="Holy See"]<-"Vatican"
subgroups$country[subgroups$country=="Netherlands (Kingdom of the)"]<-"Netherlands"

subgroups$country[subgroups$country=="Micronesia (Federated States of)"]<-"Micronesia"

#merge subgroup definition to world map dataset (with geometry)
world_map<-merge(world_map, subgroups[,c("country", "sub.region")], 
               by.x="region",by.y="country", all.x = TRUE)
#drop Antarctica
world_map<-subset(world_map, region != "Antarctica")
world_map <- world_map %>% dplyr::filter(!is.na(sub.region))
world_map<-arrange(world_map, order)

pdf(file ="graphs/S2 subregion map.pdf", height=5, width=8)
ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = world_map, 
               aes(x=long, y=lat, group=group, fill=sub.region),
               color = 'black') +
  labs(fill="Geographic region")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

dev.off()
