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
library(tidytab)
library(ggpubr)
library(egg)
library(data.table)
library(stringr)
library(lemon)
library(gtable)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#merged exposure data
lcd2025<-read.csv("output/lcd2025.csv")
data2014<-read.csv("output/data2014.csv")
data2014<-data2014[,c("ID_HDC_G0", "PopWeight_Avg_NDVI_2014_100m")]
data2016<-read.csv("output/data2016.csv")
data2016<-data2016[,c("ID_HDC_G0", "PopWeight_Avg_NDVI_2016_100m")]
data2017<-read.csv("output/data2017.csv")
data2017<-data2017[,c("ID_HDC_G0", "PopWeight_Avg_NDVI_2017_100m")]
data2018<-read.csv("output/data2018.csv")
data2018<-data2018[,c("ID_HDC_G0", "PopWeight_Avg_NDVI_2018_100m")]
data2019<-read.csv("output/data2019.csv")
data2019<-data2019[,c("ID_HDC_G0", "PopWeight_Avg_NDVI_2019_100m")]

#### merge all the yearly greenspace data sets together ####
lcd2025 <- lcd2025 %>%
  inner_join(data2014) %>% 
  inner_join(data2016) %>% 
  inner_join(data2017) %>%
  inner_join(data2018) %>%
  inner_join(data2019)

subset<-lcd2025[,c("city", "lc_group", "hdi_level", "clim_region",  "who_region", "sub.region",
                   "PopWeight_Avg_NDVI_2014_100m", "PopWeight_Avg_NDVI_2015_100m", 
                   "PopWeight_Avg_NDVI_2016_100m","PopWeight_Avg_NDVI_2017_100m", 
                   "PopWeight_Avg_NDVI_2018_100m","PopWeight_Avg_NDVI_2019_100m", 
                   "PopWeight_Avg_NDVI_2020_100m", "PopWeight_Avg_NDVI_2021_100m", 
                   "PopWeight_Avg_NDVI_2022_100m", "PopWeight_Avg_NDVI_2023_100m")]  

#reshape long so that each row represents a city/year combo               
long <- melt(setDT(subset), 
             id.vars = c("city", "lc_group", "hdi_level", "clim_region", "who_region", "sub.region"), 
             variable.name = "year")

#get rid of variable prefix/suffix so year now= "2015" etc.
long<-long %>% 
  mutate(year = str_remove(year, '^PopWeight_Avg_NDVI_'))
long<-long %>% 
  mutate(year = str_remove(year, '_100m$'))

## WHO_SUB_REGION
# create means by region/climate group
means_who_sub <- aggregate(long$value, by=list(long$sub.region, long$clim_region, long$year), 
                           function(x)mean(x, na.rm=TRUE))
# rename columns to match
colnames(means_who_sub) <- c("sub.region","clim_region","year", "mean")

#remove 20 from years so that it shows up better in graph
long$year<-gsub("^20","'",long$year)
means_who_sub$year<-gsub("^20","'",means_who_sub$year)

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
pdf(file = "graphs/Fig1 line graph averages who subregion.pdf", width=13.5, height=8.5)
a<-ggplot() +
  geom_line(data=long, aes(x=year, y=value, group=city, color=clim_region), size=.3) +
  geom_line(data=means_who_sub, aes(x=year, y=mean,group=clim_region, color=clim_region), size=1)+
  scale_color_manual(values=c("#E66101","#FDB863","#DEE6E7","#B2ABD2", "#5E3C99"), name = "Köppen-Geiger\nclimate classification")+
  xlab('Year') + ylab('Population-weighted greenest season NDVI')+ facet_wrap(~sub.region, ncol=4)+ylim(0, 0.6)+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(.6, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=14),
        strip.text = element_text(size = 14),
        axis.title=element_text(size=14)) #change region title text font size
a<-shift_legend(a)
a
dev.off()
