#########################################################################
# Table S7: HIA by region with error bars
#########################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(tidytab)
library(ggpubr)
library(egg)
library(ggridges)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#read in HIA results
regions<-read.csv("graphs/tableS2.csv")
climate<-read.csv("graphs/tableS3.csv")

regions<- regions %>%
  mutate(id = (row_number()-15)*-1)

climate<-climate[climate$clim_region!="Polar",]
climate<- climate %>%
  mutate(id = (row_number()-4)*-1)

a<-ggplot(regions) +
  geom_bar( aes(x=reorder(sub.region, id), y=mean_std, fill=sub.region), stat="identity", alpha=0.5) +
  geom_linerange( aes(x=sub.region, y=mean_std, ymin=lower_ci_std, ymax=upper_ci_std), colour="black", alpha=0.9)+
  coord_flip()+
  xlab("")+
  ylab("Attributable deaths from changes in NDVI")+
  scale_y_continuous(breaks=c(-10,-5, 0, 5, 10),
                     labels=c("10 fewer", "5 fewer", "0", "5 more", "10 more"))+
  theme(legend.position = "none")

b<-ggplot(climate) +
  geom_bar( aes(x=reorder(clim_region, id), y=mean_std, fill=clim_region), stat="identity", alpha=0.5) +
  geom_linerange( aes(x=clim_region, y=mean_std, ymin=lower_ci_std, ymax=upper_ci_std), colour="black", alpha=0.9)+
  scale_fill_brewer(palette = "PuOr")+
  coord_flip()+
  xlab("")+
  ylab("Attributable deaths from changes in NDVI")+
  scale_y_continuous(breaks=c(-10,-5, 0, 5, 10),
                     labels=c("10 fewer", "5 fewer", "0", "5 more", "10 more"))+
  theme(legend.position = "none")

#set up the file to save figure
pdf(file ="graphs/FigS7 mort by region with error.pdf")

figure <- ggarrange(a, b,
                    widths=8, heights=c(3.5,3.5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
dev.off()