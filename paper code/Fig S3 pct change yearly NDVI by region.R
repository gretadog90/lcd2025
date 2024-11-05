#########################################################################
# Line graph
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

#create a pct change over time var
lcd2025_pctChange <- subset[, c("city", "lc_group", "hdi_level", "clim_region",  "who_region", "sub.region")]
lcd2025_pctChange$pctChange2014<-0
lcd2025_pctChange$pctChange2015<-(subset$PopWeight_Avg_NDVI_2015_100m-subset$PopWeight_Avg_NDVI_2014_100m)/subset$PopWeight_Avg_NDVI_2014_100m
lcd2025_pctChange$pctChange2016<-(subset$PopWeight_Avg_NDVI_2016_100m-subset$PopWeight_Avg_NDVI_2015_100m)/subset$PopWeight_Avg_NDVI_2015_100m
lcd2025_pctChange$pctChange2017<-(subset$PopWeight_Avg_NDVI_2017_100m-subset$PopWeight_Avg_NDVI_2016_100m)/subset$PopWeight_Avg_NDVI_2016_100m
lcd2025_pctChange$pctChange2018<-(subset$PopWeight_Avg_NDVI_2018_100m-subset$PopWeight_Avg_NDVI_2017_100m)/subset$PopWeight_Avg_NDVI_2017_100m
lcd2025_pctChange$pctChange2019<-(subset$PopWeight_Avg_NDVI_2019_100m-subset$PopWeight_Avg_NDVI_2018_100m)/subset$PopWeight_Avg_NDVI_2018_100m
lcd2025_pctChange$pctChange2020<-(subset$PopWeight_Avg_NDVI_2020_100m-subset$PopWeight_Avg_NDVI_2019_100m)/subset$PopWeight_Avg_NDVI_2019_100m
lcd2025_pctChange$pctChange2021<-(subset$PopWeight_Avg_NDVI_2021_100m-subset$PopWeight_Avg_NDVI_2020_100m)/subset$PopWeight_Avg_NDVI_2020_100m
lcd2025_pctChange$pctChange2022<-(subset$PopWeight_Avg_NDVI_2022_100m-subset$PopWeight_Avg_NDVI_2021_100m)/subset$PopWeight_Avg_NDVI_2021_100m
lcd2025_pctChange$pctChange2023<-(subset$PopWeight_Avg_NDVI_2023_100m-subset$PopWeight_Avg_NDVI_2022_100m)/subset$PopWeight_Avg_NDVI_2022_100m

#reshape long so that each row represents a city/year combo               
long_pctChange <- melt(setDT(lcd2025_pctChange), 
                       id.vars = c("city", "lc_group", "hdi_level", "clim_region", "who_region", "sub.region"), 
                       variable.name = "year")

#get rid of variable prefix/suffix so year now= "2015" etc.
long_pctChange<-long_pctChange %>% 
  mutate(year = str_remove(year, '^pctChange'))
long_pctChange$value<-long_pctChange$value*100

means_pctChange <- aggregate(long_pctChange$value, 
                             by=list(long_pctChange$sub.region, long_pctChange$clim_region, long_pctChange$year), 
                             function(x)mean(x, na.rm=TRUE))
# rename columns to match
colnames(means_pctChange) <- c("sub.region","clim_region","year", "mean")

#remove 20 from years so that it shows up better in graph
long_pctChange$year<-gsub("^20","'",long_pctChange$year)
means_pctChange$year<-gsub("^20","'",means_pctChange$year)

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
pdf(file = "graphs/line graph pct change who subregion set scale.pdf", width=13.5, height=8.5)
a<-ggplot() +
  geom_line(data=long_pctChange, aes(x=year, y=value, group=city, color=clim_region), size=.3) +
  geom_line(data=means_pctChange, aes(x=year, y=mean,group=clim_region, color=clim_region), size=1)+
  scale_color_brewer(palette = "PuOr", name = "Köppen-Geiger\nclimate classification")+
  xlab('Year') + ylab('Population-weighted greenest season NDVI')+ ylim(-50, 50)+facet_wrap(~sub.region, ncol=4)+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(.6, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=13), #change legend title font size
        legend.text = element_text(size=12),
        strip.text = element_text(size = 12),
        axis.title=element_text(size=13)) #change legend text font size
a<-shift_legend(a)
a
dev.off()
