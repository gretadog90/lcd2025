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

ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = world_map, 
               aes(x=long, y=lat, group=group, fill=sub.region),
               color = 'black') +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))


#map the subregions
ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = world_map,
               aes(x=long, y=lat, group = group)) +
  ## Second layer: Country map
  geom_polygon(data = world_map,
               aes(x=long, y=lat, group=group, fill=sub.region)) 

+
  theme(legend.position="none")


+
  geom_polygon(data = hia, aes(x = Longitude, y = Latitude, 
                             colour = cut(ndvi2014_2018, c(0, .1, .2, .3, .4, .5, 1))), size=.5) +
  scale_color_manual(name = "2014-2018\nPopulation-weighted\nGreenest Season NDVI",
                     values=c("#EEB99F","#EAB64E","#E6E600","#63C600","#3b7600","#003100", "#F2F2F2"),
                     labels=c("0-0.09", "0.10-0.19", "0.20-0.29", "0.30-0.39", "0.40-0.49", "0.5+")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))


[1] "British Indian Ocean Territory"                      
[2] "French Southern Territories"                         
[3] "Réunion" 
[7] "Cabo Verde"  
[9] "Antigua and Barbuda"                                 
[10] "Bonaire, Sint Eustatius and Saba"                    
[11] "British Virgin Islands"                              
[12] "Curaçao"                                             
[13] "Saint Barthélemy"                                    
[14] "Saint Kitts and Nevis"                               
[15] "Saint Martin (French Part)"                          
[16] "Saint Vincent and the Grenadines"                    
[17] "Sint Maarten (Dutch part)"   
[19] "United States Virgin Islands"     
[21] "Bouvet Island"                                       
[22] "Falkland Islands (Malvinas)"                         
[23] "South Georgia and the South Sandwich Islands"    
[40] "Åland Islands"                                       
[41] "Svalbard and Jan Mayen Islands" 
[43] "Gibraltar"  
[46] "Cocos (Keeling) Islands"                             
[47] "Heard Island and McDonald Islands"  
[49] "United States Minor Outlying Islands"                
[50] "Pitcairn"   
[53] "Wallis and Futuna Islands" 
[51] "Tokelau"  
[52] "Tuvalu"  
