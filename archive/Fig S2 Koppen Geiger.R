#########################################################################
#Figure S3. Map of the world by Köppen-Geiger climate classification. 
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
library(sf)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#read in data (fixed width text file)
u<-'groupings/Koeppen-Geiger-ASCII.txt'
kg_classifications<-read.table(text = gsub("  +", ",", readLines(u)), sep = ",")

#map of world
world_map = map_data('world')

#columns didn't read in so rename and drop first row and blank column
colnames(kg_classifications) <- c("drop", "lat", "long", "class_specific")
kg_classifications = kg_classifications[-1,]

#create broad categories used in paper from smaller ones
kg_classifications["class"]<-"Continental"
kg_classifications$class[kg_classifications$class_specific %in% c("Af", "Am", "Aw", "As")]<-"Tropical"
kg_classifications$class[kg_classifications$class_specific %in% c("BSh", "BSk", "BWh", "BWk")]<-"Arid"
kg_classifications$class[kg_classifications$class_specific %in% c("Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc")]<-"Temperate"
kg_classifications$class[kg_classifications$class_specific %in% c("ET", "EF")]<-"Polar"
table(kg_classifications$class_specific)
table(kg_classifications$class)

#convert lat/long from string to numeric
kg_classifications$lat<-as.numeric(kg_classifications$lat)
kg_classifications$long<-as.numeric(kg_classifications$long)

my_sf <- st_as_sf(kg_classifications, coords = c('long', 'lat'))
polygon <- kg_classifications %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")


pdf(file ="graphs/S3 Koppen Geiger map.pdf", height=5, width=8)


ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_sf(data = my_sf, 
               aes(colour=class), size=.5)


+
  labs(fill="Köppen-Geiger\nclimate classification")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

dev.off()



# create data for world coordinates using  
# map_data() function 
world_coordinates <- map_data("world") 

# create world map using ggplot() function 
ggplot() + 
  
  # geom_map() function takes world coordinates  
  # as input to plot world map 
  geom_map( 
    data = kg_classifications, map = kg_classifications, 
    aes(long, lat, map_id = class) 
  )


#merge subgroup definition to world map dataset (with geometry)
world_map<-merge(world_map, kg_classifications[,c("lat", "long", "class")], 
                 by=c("lat","long"), all.x = TRUE)

#drop Antarctica
world_map<-subset(world_map, region != "Antarctica")
#world_map <- world_map %>% dplyr::filter(!is.na(sub.region))
world_map<-arrange(world_map, order)