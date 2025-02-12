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

hia$delta_mortality<-(hia$delta_mortality/hia$Population_2020_100m)*100000
summary(hia$delta_mortality)
hist(hia$delta_mortality)
hia$a<-(hia$lb/hia$Population_2020_100m)*100000
hia$b<-(hia$ub/hia$Population_2020_100m)*100000
hia <- transform(hia, lb = pmin(a, b))
hia <- transform(hia, ub = pmax(a, b))
summary(hia$lb)
summary(hia$ub)

hia$lb_str<-as.character(round(hia$lb, 2))
hia$ub_str<-as.character(round(hia$ub, 2))
hia$ci<-paste("(",hia$lb_str,", ",hia$ub_str,")", sep="")
summary<-hia[,c("city", "country", "sub.region", "clim_region", "delta_mortality", "ci")]

summary<-summary[order(summary$sub.region, summary$city),]

write.csv(summary, "graphs/tableS1.csv")

region_sum<-hia %>% 
  group_by(sub.region) %>% 
  summarize(
    delta_mortality = mean(delta_mortality),
    lb = mean(lb),
    ub=mean(ub),
  )

region_sum<- region_sum %>%
  mutate(id = (row_number()-15)*-1)

climate_sum<-hia %>% 
  group_by(clim_region) %>% 
  summarize(
    delta_mortality = mean(delta_mortality),
    lb = mean(lb),
    ub=mean(ub),
  )

climate_sum<-climate_sum[climate_sum$clim_region!="Polar",]
climate_sum<- climate_sum %>%
  mutate(id = (row_number()-4)*-1)

a<-ggplot(region_sum) +
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
pdf(file ="graphs/Fig S7 mort by region with error.pdf")

figure <- ggarrange(a, b,
                    widths=8, heights=c(3.5,3.5),
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
dev.off()