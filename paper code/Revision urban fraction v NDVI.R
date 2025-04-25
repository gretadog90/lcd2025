###

# compare % urban from landcover dataset to NDVI 2015 and 2020

###

#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(viridis)

#set working directory
setwd('~/Documents/data/Lancet 2025/')

#import all the years of 100m peak NDVI
data2014<-read.csv("output/data2014.csv")
data2014<-data2014[,c("country", "city", "ID_HDC_G0", "PopWeight_Peak_NDVI_2014_100m")]
data2014<-data2014[!is.na(data2014$PopWeight_Peak_NDVI_2014_100m),]
data2015<-read.csv("output/data2015.csv")
data2015<-data2015[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2015_100m")]
data2016<-read.csv("output/data2016.csv")
data2016<-data2016[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2016_100m")]
data2017<-read.csv("output/data2017.csv")
data2017<-data2017[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2017_100m")]
data2018<-read.csv("output/data2018.csv")
data2018<-data2018[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2018_100m")]
data2019<-read.csv("output/data2019.csv")
data2019<-data2019[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2019_100m")]
data2020<-read.csv("output/data2020.csv")
data2020<-data2020[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2020_100m")]
data2021<-read.csv("output/data2021.csv")
data2021<-data2021[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2021_100m")]
data2022<-read.csv("output/data2022.csv")
data2022<-data2022[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2022_100m")]
data2023<-read.csv("output/data2023.csv")
data2023<-data2023[,c("ID_HDC_G0", "PopWeight_Peak_NDVI_2023_100m")]


urban2014<-read.csv("urban fraction/2014_urban.csv")
urban2014<-urban2014[,c("ID_HDC_G0", "mean")]
names(urban2014) <- c("ID_HDC_G0", "mean_2014")
urban2015<-read.csv("urban fraction/2015_urban.csv")
urban2015<-urban2015[,c("ID_HDC_G0", "mean")]
names(urban2015) <- c("ID_HDC_G0", "mean_2015")
urban2016<-read.csv("urban fraction/2016_urban.csv")
urban2016<-urban2016[,c("ID_HDC_G0", "mean")]
names(urban2016) <- c("ID_HDC_G0", "mean_2016")
urban2017<-read.csv("urban fraction/2017_urban.csv")
urban2017<-urban2017[,c("ID_HDC_G0", "mean")]
names(urban2017) <- c("ID_HDC_G0", "mean_2017")
urban2018<-read.csv("urban fraction/2018_urban.csv")
urban2018<-urban2018[,c("ID_HDC_G0", "mean")]
names(urban2018) <- c("ID_HDC_G0", "mean_2018")
urban2019<-read.csv("urban fraction/2019_urban.csv")
urban2019<-urban2019[,c("ID_HDC_G0", "mean")]
names(urban2019) <- c("ID_HDC_G0", "mean_2019")
urban2020<-read.csv("urban fraction/2020_urban.csv")
urban2020<-urban2020[,c("ID_HDC_G0", "mean")]
names(urban2020) <- c("ID_HDC_G0", "mean_2020")
urban2021<-read.csv("urban fraction/2021_urban.csv")
urban2021<-urban2021[,c("ID_HDC_G0", "mean")]
names(urban2021) <- c("ID_HDC_G0", "mean_2021")
urban2022<-read.csv("urban fraction/2022_urban.csv")
urban2022<-urban2022[,c("ID_HDC_G0", "mean")]
names(urban2022) <- c("ID_HDC_G0", "mean_2022")
urban2023<-read.csv("urban fraction/2023_urban.csv")
urban2023<-urban2023[,c("ID_HDC_G0", "mean")]
names(urban2023) <- c("ID_HDC_G0", "mean_2023")

ndvi <- data2014 %>%
  inner_join(data2015) %>% 
  inner_join(data2016) %>% 
  inner_join(data2017) %>% 
  inner_join(data2018) %>% 
  inner_join(data2019) %>% 
  inner_join(data2020) %>% 
  inner_join(data2021) %>% 
  inner_join(data2022) %>% 
  inner_join(data2023)

names(ndvi)
names(ndvi) <- sub('_100m', '', names(ndvi))

ndvi_long <- ndvi %>% 
  pivot_longer(
    cols = -c("city", "country", "ID_HDC_G0"), 
    names_to = "year",
    names_prefix =  c("PopWeight_Peak_NDVI_"),
    values_to = c("ndvi")
  )

urban_fraction<-urban2014 %>% 
  inner_join(urban2015) %>% 
  inner_join(urban2016) %>% 
  inner_join(urban2017) %>% 
  inner_join(urban2018) %>% 
  inner_join(urban2019) %>% 
  inner_join(urban2020) %>% 
  inner_join(urban2021) %>% 
  inner_join(urban2022) %>% 
  inner_join(urban2023)

urban_long <- urban_fraction %>% 
  pivot_longer(
    cols = -c("ID_HDC_G0"), 
    names_to = "year",
    names_prefix =  c("mean_"),
    values_to = c("urban")
  )

long<-ndvi_long  %>%
  inner_join(urban_long)

cor(long$ndvi, long$urban, method="pearson")

pdf(file = "graphs/FigS7 correlation.pdf", width=9, height=8)
ggplot(long) +
  geom_point(aes(x=ndvi, y=urban, color=year))+scale_color_viridis(discrete = TRUE,"magma")+
  xlab("NDVI")+ylab("Urban fraction")+ 
  guides(color = guide_legend(title = "Year"))+
  geom_smooth(aes(x=ndvi, y=urban), method = "lm", se=FALSE, color='black')+
  annotate(geom="text", x=0.55, y=0.55, label="corr=-0.312",color="black")
dev.off()

long$year<-as.numeric(long$year)
cor(long$year, long$urban, method="pearson")

long$year<-as.numeric(long$year)
regional<-read.csv("output/lcd2025.csv")
regional<-regional[,c("ID_HDC_G0", "sub.region")]
long<-merge(long, regional, "ID_HDC_G0")

regional_urban<- long %>%
  group_by(sub.region, year) %>%
  summarise(urban = mean(urban))

pdf(file = "graphs/FigS8 urbanization year.pdf", width=9, height=8)
ggplot(regional_urban) +
  geom_point(aes(x=year, y=urban, color=sub.region))+
  xlab("Year")+ylab("Urban fraction")+ 
  guides(color = guide_legend(title = "Geographic region"))+
  geom_smooth(aes(x=year, y=urban), method = "lm", se=FALSE, color='black')
dev.off()


cor(long$year, long$urban, method="pearson")
