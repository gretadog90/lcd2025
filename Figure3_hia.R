#########################################################################
# Figure 3: HIA 2020 v 2015 and 2023 v 2015
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
hia<-read.csv("outputHIA/hia_100m.csv")
hist(hia$e2020_all2015)

hia$e2020_all2015<-(hia$e2020_all2015/hia$Population_2015_100m)*100000
summary(hia$e2020_all2015)
 colnames(hia)
hia$e_2023_2020<-(hia$e_2023_2020/hia$Population_2020_100m)*100000
summary(hia$e_2023_2020)

pdf(file = "graphs/Figure3.pdf")

#panel a: 2015-2020 HIA results (2015 pop/mort)
panela<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = hia, aes(x = Longitude, y = Latitude, 
                             colour=cut(e2020_all2015, c(-Inf, -10, -5, -1, 1, 5, 10, Inf))), size=1) +
  scale_color_manual(name = "Change in Number\nof Deaths per 100,000\n2015 to 2020", 
                     values=c( "#2166AC", "#4393C3", "#92C5DE", "#f2f2f2","#F4A582", "#D6604D", "#B2182B"),
                     labels=c("10+ fewer", "5+ fewer", "1+ fewer","less than 1 change", 
                              "1+ more", "5+ more", "10+ more"))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

#panel b: 2020-2023 HIA results (2020 pop/mort)
panelb<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = hia, aes(x = Longitude, y = Latitude, 
                             colour=cut(e_2023_2020, c(-Inf, -10, -5, -1, 1, 5, 10, Inf))), size=1) +
  scale_color_manual(name = "Change in Number\nof Deaths per 100,000\n2015 to 2020", 
                     values=c( "#2166AC", "#4393C3", "#92C5DE", "#f2f2f2","#F4A582", "#D6604D", "#B2182B"),
                     labels=c("10+ fewer", "5+ fewer", "1+ fewer","less than 1 change", 
                              "1+ more", "5+ more", "10+ more"))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))


#set up the file to save figure
pdf(file = "graphs/Figure3.pdf")

figure <- ggarrange(panela, panelb, 
                    widths =8, heights=c(5,5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

dev.off()

#panel a: 2015-2020 HIA results (2015 pop/mort)
panela<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = hia, aes(x = Longitude, y = Latitude, 
                             colour=cut(e2020_all2015, c(-Inf, -20, -10, -5, 5, 10, 20, Inf))), size=1) +
  scale_color_manual(name = "Change in Number\nof Deaths per 100,000\n2015 to 2020", 
                     values=c( "#2166AC", "#4393C3", "#92C5DE", "#f2f2f2","#F4A582", "#D6604D", "#B2182B"),
                     labels=c("20+ fewer", "10+ fewer", "5+ fewer","less than 5 change", 
                              "5+ more", "10+ more", "20+ more"))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

#panel b: 2020-2023 HIA results (2020 pop/mort)
panelb<-ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",], #drop Antarctica
               aes(x=long, y=lat, group = group)) + #get the country outlines
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region != "Antarctica",],
               aes(x=long, y=lat, group=group),
               color = 'black', fill = 'white') +
  geom_point(data = hia, aes(x = Longitude, y = Latitude, 
                             colour=cut(e_2023_2020, c(-Inf, -20, -10, -5, 5, 10, 20, Inf))), size=1) +
  scale_color_manual(name = "Change in Number\nof Deaths per 100,000\n2015 to 2020", 
                     values=c( "#2166AC", "#4393C3", "#92C5DE", "#f2f2f2","#F4A582", "#D6604D", "#B2182B"),
                     labels=c("20+ fewer", "10+ fewer", "5+ fewer","less than 5 change", 
                              "5+ more", "10+ more", "20+ more"))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))


#set up the file to save figure
pdf(file = "graphs/Figure3.pdf")

figure <- ggarrange(panela, panelb, 
                    widths =8, heights=c(5,5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)

dev.off()
