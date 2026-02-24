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
#library(ggpubr)
library(egg)
library(data.table)
library(stringr)
library(lemon)
library(gtable)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#merged exposure data
setwd('~/Documents/data/Lancet 2025/output')

#import the merged dataset
lcd2025<-read.csv("lcd2025.csv")
lcd2025<- lcd2025[-which(lcd2025$city=='Kapoeta'),]

#subset to needed vars
subset<-lcd2025[,c("city", "lc_group", "hdi_level", "clim_region",  "who_region", "sub.region",
                   "PopWeight_Peak_NDVI_2015_100m", "PopWeight_Peak_NDVI_2016_100m",
                   "PopWeight_Peak_NDVI_2017_100m", "PopWeight_Peak_NDVI_2018_100m",
                   "PopWeight_Peak_NDVI_2019_100m", "PopWeight_Peak_NDVI_2020_100m", 
                   "PopWeight_Peak_NDVI_2021_100m", "PopWeight_Peak_NDVI_2022_100m", 
                   "PopWeight_Peak_NDVI_2023_100m", "PopWeight_Peak_NDVI_2024_100m",
                   "PopWeight_Peak_NDVI_2025_100m")]  

#reshape long so that each row represents a city/year combo               
long <- melt(setDT(subset), 
             id.vars = c("city", "lc_group", "hdi_level", "clim_region", "who_region", "sub.region"), 
             variable.name = "year")

#get rid of variable prefix/suffix so year now= "2015" etc.
long<-long %>% 
  mutate(year = str_remove(year, '^PopWeight_Peak_NDVI_'))
long<-long %>% 
  mutate(year = str_remove(year, '_100m$'))

## LC_GROUP
# create means by lc_group
means_lc <- aggregate(long$value, by=list(long$lc_group, long$hdi_level, long$year), 
                           function(x)mean(x))
# rename columns to match
colnames(means_lc) <- c("lc_group","hdi_level","year", "mean")

#remove 20 from years so that it shows up better in graph
long$year<-gsub("^20","'",long$year)
means_lc$year<-gsub("^20","'",means_lc$year)

#make hdi into a factor var with a more sensible order
long$hdi_level <- factor(long$hdi_level, levels = c("Very High", "High", "Medium", "Low", "N/A"))

#function to select location for legend
shift_legend <- function(p) {
  # ...
  # to grob
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  
  # now we just need a simple call to reposition the legend
  reposition_legend(p, 'center', panel=names)
}

#plot
pdf(file = "line graph lc group hdi.pdf", width=13.5, height=8.5)
a<-ggplot() +
  geom_line(data=long, aes(x=year, y=value, group=city, color=hdi_level), size=.3) +
  scale_color_manual(values=c("Very High"="purple4","High"="deepskyblue4","Medium"="mediumseagreen",
                              "Low"="gold", "NA"="gray42"), name = "HDI level")+
  xlab('Year') + ylab('Population-weighted greenest season NDVI')+ facet_wrap(~sub.region, ncol=4)+ylim(0, 0.6)+
  theme(legend.key.size = unit(5, 'cm'), #change legend key size
        legend.key.height = unit(.6, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=14),
        strip.text = element_text(size = 14),
        axis.title=element_text(size=14)) #change region title text font size 
        
a<-a+guides(linetype = guide_legend(override.aes = list(linewidth = 10)))
a<-shift_legend(a)
a
dev.off()

