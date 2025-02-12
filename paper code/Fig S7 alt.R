#########################################################################
# Table S7: HIA by region with error bars
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
library(ggridges)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#read in HIA results
hia<-read.csv("outputHIA/hia_100m_5yr.csv")
hia<-hia[!is.na(hia$ndvi2019_2023),]
hist(hia$delta_mortality)

hia_sum<- hia %>% 
  group_by(sub.region) %>% 
  summarize(
    delta_mortality = sum(delta_mortality),
    pop = sum(Population_2020_100m),
    lb=sum(lb),
    ub=sum(ub)
  )

hia_sum$delta_mortality<-(hia_sum$delta_mortality/hia_sum$pop)*100000
hia_sum$a<-(hia_sum$lb/hia_sum$pop)*100000
hia_sum$b<-(hia_sum$ub/hia_sum$pop)*100000
hia_sum <- transform(hia_sum, lb = pmin(a, b))
hia_sum <- transform(hia_sum, ub = pmax(a, b))

hia_sum<- hia_sum %>%
  mutate(id = (row_number()-15)*-1)

climate_sum<-hia %>% 
  group_by(clim_region) %>% 
  summarize(
    delta_mortality = sum(delta_mortality),
    pop = sum(Population_2020_100m),
    lb=sum(lb),
    ub=sum(ub)
  )

climate_sum$delta_mortality<-(climate_sum$delta_mortality/climate_sum$pop)*100000
climate_sum$a<-(climate_sum$lb/climate_sum$pop)*100000
climate_sum$b<-(climate_sum$ub/climate_sum$pop)*100000
climate_sum <- transform(climate_sum, lb = pmin(a, b))
climate_sum <- transform(climate_sum, ub = pmax(a, b))

climate_sum<-climate_sum[climate_sum$clim_region!="Polar",]
climate_sum<- climate_sum %>%
  mutate(id = (row_number()-4)*-1)

a<-ggplot(hia_sum) +
  geom_bar( aes(x=sub.region, y=delta_mortality, fill=sub.region), stat="identity", alpha=0.5) +
  geom_linerange( aes(x=sub.region, y=delta_mortality, ymin=lb, ymax=ub), colour="black", alpha=0.9)+
  coord_flip()+
  xlab("")+
  ylab("Attributable deaths from changes in NDVI")+
  scale_y_continuous(breaks=c(-10,-5, 0, 5, 10),
                     labels=c("10 fewer", "5 fewer", "0", "5 more", "10 more"))+
  theme(legend.position = "none")

b<-ggplot(climate_sum) +
  geom_bar( aes(x=clim_region, y=delta_mortality, fill=clim_region), stat="identity", alpha=0.5) +
  geom_linerange( aes(x=clim_region, y=delta_mortality, ymin=lb, ymax=ub), colour="black", alpha=0.9)+
  scale_fill_brewer(palette = "PuOr")+
  coord_flip()+
  xlab("")+
  ylab("Attributable deaths from changes in NDVI")+
  scale_y_continuous(breaks=c(-10,-5, 0, 5, 10),
                     labels=c("10 fewer", "5 fewer", "0", "5 more", "10 more"))+
  theme(legend.position = "none")

#set up the file to save figure
pdf(file ="graphs/Fig S7 alt.pdf")

figure <- ggarrange(a, b,
                    widths=8, heights=c(3.5,3.5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
dev.off()
                    