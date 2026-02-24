#########################################################################
# Update HDI supplemental figure
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
library(ggpubr)
library(egg)
library(readxl)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')

#import data
country_lcd<-read_excel("groupings/2026 Guidance_Country Names and Groupings.xlsx", 
                        range = "A2:E222")
data<- map_data('world')[map_data('world')$region != "Antarctica",]

#remove extra info from names in lcd file for better matching
country_lcd$region<-str_remove(country_lcd$`Country Name to use`, "\\s*\\([^\\)]+\\)")

#figure out where names don't match
no_match<-unique(data$region[!data$region %in% country_lcd$region]) 
print(no_match)

#where there is a match but diff string, replace string
data$region[data$region %in% c("Antigua", "Barbuda")]<-"Antigua and Barbuda"
data$region[data$region=="Brunei"]<-"Brunei Darussalam"
data$region[data$region=="Ivory Coast"]<-"Cote d'Ivoire"
data$region[data$region=="Republic of Congo"]<-"Congo"
data$region[data$region=="Cape Verde"]<-"Cabo Verde"
data$region[data$region=="Curacao"]<-"Curaçao"
data$region[data$region=="Czech Republic"]<-"Czechia"
data$region[data$region=="Micronesia"]<-"Federated States of Micronesia"
data$region[data$region=="UK"]<-"United Kingdom"
data$region[data$region=="Guinea-Bissau"]<-"Guinea Bissau"
data$region[data$region=="Iran"]<-"Islamic Republic of Iran"
data$region[data$region %in% c("Nevis", "Saint Kitts")]<-"Saint Kitts and Nevis"
data$region[data$region=="South Korea"]<-"Republic of Korea"
data$region[data$region=="Laos"]<-"Lao People's Democratic Republic"
data$region[data$region=="Moldova"]<-"Republic of Moldova"
data$region[data$region=="North Korea"]<-"Democratic People's Republic of Korea"
data$region[data$region=="Palestine"]<-"Occupied Palestinian territory"
data$region[data$region=="Russia"]<-"Russian Federation"
data$region[data$region=="Swaziland"]<-"Eswatini"
data$region[data$region=="Syria"]<-"Syrian Arab Republic"
data$region[data$region %in% c("Trinidad", "Tobago")]<-"Trinidad and Tobago"
data$region[data$region=="Turkey"]<-"Türkiye"
data$region[data$region=="Tanzania"]<-"United Republic of Tanzania"
data$region[data$region=="USA"]<-"United States of America"
data$region[data$region %in% c("Grenadines", "Saint Vincent")]<-"Saint Vincent and the Grenadines"
data$region[data$region=="Venezuela"]<-"Bolivarian Republic of Venezuela"
data$region[data$region=="Turks and Caicos Islands"]<-"Turks and Caicos"

#make sure all were entered correctly
no_match<-unique(data$region[!data$region %in% country_lcd$region]) 
print(no_match)

#rename to easier vars
country_lcd <- country_lcd %>%
  dplyr::rename(country=`Country Name to use`,
                hdi_level=`HDI Group 2025`)

#merge on hdi_level
graph_data<- merge(data, country_lcd, by.x="region", by.y="country")

table(graph_data$hdi_level)

#replace missing with N/A and factor to set order of legend
graph_data$hdi_level[is.na(graph_data$hdi_level)] <- "No Data"
graph_data$hdi_level[graph_data$hdi_level=="N/A"] <- "No Data"

graph_data$hdi_level <- factor(graph_data$hdi_level,
                              ordered = TRUE,
                              levels = c("Low", "Medium", "High", "Very High", "No Data"))
                 
pdf(file ="HDI level.pdf", height=4, width=9)

#plot
ggplot(graph_data) + 
  geom_map(dat=data, map=data, 
           aes(map_id=region), fill="white", color="black") + 
  geom_map(map=data, 
           aes(map_id=region, fill=hdi_level), color="black") + 
  expand_limits(x = data$long, y = data$lat) +
  labs(fill="Human Development Index")+
  theme_classic()+
  scale_fill_manual(values = c("#341539", "dodgerblue3", "mediumseagreen", "yellow2", "grey"))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.text.y =element_blank(),
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),  axis.line=element_blank())

dev.off()
