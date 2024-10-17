#########################################################################
# Figure 2: Green+blue area 2015 & 2020
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
library(data.table)
library(ggforce)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#merged exposure data
lcd2025<-read.csv("output/lcd2025.csv")
who_region<-read.csv("groupings/who_regions.csv")

#generate an urban area var that will be 1-green or blue area
lcd2025$Urban_Area_2015<-1-lcd2025$GreenBlue_Area_2015
lcd2025$Urban_Area_2020<-1-lcd2025$GreenBlue_Area_2020
lcd2025$Blue_Area_2015<-lcd2025$GreenBlue_Area_2015-lcd2025$Green_Area_2015
lcd2025$Blue_Area_2020<-lcd2025$GreenBlue_Area_2020-lcd2025$Green_Area_2020

lcd2025<-merge(lcd2025, who_region[,c("alpha.3", "sub.region")], 
               by.x="ISO3",by.y="alpha.3", all.x = TRUE)

#just verifying they all sum to 1
lcd2025$total<-lcd2025$Green_Area_2020+lcd2025$Blue_Area_2020+lcd2025$Urban_Area_2020
tab(lcd2025$total)

#subset to just the needed vars for the reshape
subset2020<-lcd2025[,c("city", "ID_HDC_G0", "sub.region",
                           "Green_Area_2020", "Blue_Area_2020", "Urban_Area_2020",
                           "Population_2020_100m","lc_group","clim_region", "hdi_level")]

subset2020<-subset2020[order(subset2020$sub.region, subset2020$Green_Area_2020, subset2020$Blue_Area_2020),]
subset2020$order<- seq.int(nrow(subset2020))

#reshape so that new "type" var is blue/green/urban and value is proportion
long <- melt(setDT(subset2020), id.vars = c("city","ID_HDC_G0", "lc_group", "clim_region", "order", "sub.region",
                                                "hdi_level", "Population_2020_100m"), 
             variable.name = "type")
tab(long$type)

pdf(file = "graphs/Figure2 gba.pdf", width=15, height=4)

#bar graph with separate plots for each region
ggplot(long, aes(fill=type, y=value, x=order)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual("Land type", 
                    values = c("Green_Area_2020" = "darkgreen", "Blue_Area_2020" = "dodgerblue", "Urban_Area_2020" = "grey"),
                    labels=c("Green area", "Blue area", "Urban/non-\nvegetated area"))+
  ggforce::facet_row(
    facets = vars(sub.region),
    space  = "free",
    scales="free_x",
    strip.position = "left"
  ) +labs(x = "city", y = "Proportion")+
  theme(axis.ticks.x = element_blank(), axis.text.x =element_blank(),
        legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "white"))
dev.off()
expand=c(0,0)