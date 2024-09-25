#########################################################################
# Create tables and maps for Lancet Countdown.
#########################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(purrr)
library(maps)
library(ggplot2) 

####import####
#import all the yearly data files
#set working directory
setwd('~/Documents/data/Lancet 2025/output')
output='~/Documents/data/Lancet 2025/output/tables and graphs/'

#### Load in merged data set ####
merged_data_2025<-read.csv("merged_data_2025.csv")

#### Main text tables/figures ####
#Figure 1: Urban greenness in urban centers with more than 500,000 inhabitants in 2023.
map<-map(database = "world") 
points(x = merged_data_2025$Longitude, y =merged_data_2025$Latitude, col ="Green")

#Figure 2: Mean, population-weighted peak-season NDVI by HDI and year. 
aggregate(data_all_hdi_modgreen$max_popweighted_2023,by=list(data_all_hdi_modgreen$hdi_level), FUN='mean')
ggplot(merged_data_2025, aes(fill=year, y=PopWeight_Peak_NDVI_2023, x=hdi_level)) + 
  geom_bar(position="dodge", stat="identity")

#### Supplemental text tables/figures ####
#Table 1: Categorization of Greenness Levels
table1 <- data.frame(
  level = c('Exceptionally Low','Very Low','Low','Moderate', 'Exceptionally High','High', 'Very High'),
  values = c("<0.2","0.20-0.29","0.30-0.39","0.40-0.49","0.50-0.59", "0.60-0.69", "???0.70")
)
names(table1) <- c("Level of Greenness", "Population-Weighted Peak NDVI")
write.csv(table1, paste0(output, "table1.csv"))

#Table 2: Global percent moderate or above (population-weighted average peak-season NDVI ??? 0.40)
#% and (number/total)
data_all_hdi_modgreen<-data_all %>%
  filter (indicator_2023 == "Moderate"|indicator_2023=="High"|
            indicator_2023=="Very High") 

data_all_hdi_modgreen_2022<-table(data_all_hdi_modgreen$indicator_2022,data_all_hdi_modgreen$hdi_level)
data_all_hdi_modgreen_2022<-as.data.frame(data_all_hdi_modgreen_2022)
aggregate(data_all_hdi_modgreen$max_popweighted_2022,by=list(data_all_hdi_modgreen$hdi_level), FUN='mean')
#Table 3: Global average population-weighted peak-season NDVI
#Table 4: Population-weighted peak-season NDVI by HDI 
#(2015-2023)
#Table 5: Population-weighted peak-season NDVI by climate region
#(2015-2023)
#Table 6: Population-weighted peak-season NDVI by WHO region
#(2015-2023)
#Table 7: Population-weighted peak-season NDVI by Lancet Countdown region
#(2015-2023)
#Table 8: Percent moderate or above by HDI (population-weighted average peak-season NDVI ???0.40)
#(2015-2023), % and (number/total)
#Table 9: Climate region percent moderate or above (population-weighted average peak-season NDVI ???0.40)
#(2015-2023), % and (number/total)
#Table 10 Percent moderate or above by WHO region (population-weighted average peak-season NDVI ???0.40)
#(2015-2023), % and (number/total)
#Table 11 Percent moderate or above by Lancet Countdown region (population-weighted average peak-season NDVI ???0.40)
#(2015-2023), % and (number/total)
#Table 12: Estimates of Urban Green Space by HDI (2015)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 13: Estimates of Urban Green Space by HDI (2020)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 14: Estimates of Urban Green Space by HDI (2021)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 15: Estimates of Urban Green Space by HDI (2022)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 16: Estimates of Urban Green Space by HDI (2023)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 17: Estimates of Urban Green Space by Climate Region (2015)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 18: Estimates of Urban Green Space by Climate Region (2020)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 19: Estimates of Urban Green Space by Climate Region (2021)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 20: Estimates of Urban Green Space by Climate Region (2022)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 21: Estimates of Urban Green Space by Climate Region (2023)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 22: Estimates of Urban Green Space by WHO region  (2015)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 23: Estimates of Urban Green Space by WHO region  (2020)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 24: Estimates of Urban Green Space by WHO region  (2021)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 25: Estimates of Urban Green Space by WHO region  (2022)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 26: Estimates of Urban Green Space by WHO region  (2023)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 27: Estimates of Urban Green Space by LCD region (2015)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 28: Estimates of Urban Green Space by LCD region (2020)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 29: Estimates of Urban Green Space by LCD region (2021)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 30: Estimates of Urban Green Space by LCD region (2022)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Table 31: Estimates of Urban Green Space by LCD region (2023)
#Peak NDVI, Four-season NDVI, Pop. weighted Peak NDVI, Pop. weighted Four-season NDVI
#Figure 1. K??ppen Climate Regions. Designated climate regions of the world using the Koppen Climate Zones system.
#Figure 2. Country Development Level. Development level as denoted by the Human Development Index (HDI).
#Figure 3. World Health Organization (WHO) regions.
#Figure 4. Lancet countdown regions and urban greenness.
#Figure 5. Urban greenness in 1,042 urban centres in 2023. Levels of urban greenness 
#were quantified on the basis of mean population-weighted peak-season normalized difference 
#vegetation index (NDVI). The NDVI is a standard, satellite-based measurement used to estimate 
#vegetation on a scale of -1.0 to 1.0.
#Figure 6. Temporal changes in urban greenness. Levels of urban greenness change between 2015, 2020, and 2023.
#Figure 7. Percentage of urban centres by urban greenness level over multiple years.
#Figure 8. Mean, population-weighted peak-season NDVI by climate region and year.
#Figure 9. Mean, population-weighted peak-season NDVI by HDI and year.
#Figure 10. Mean, population-weighted peak-season NDVI by WHO region and year.
#Figure 11. Mean, population-weighted peak-season NDVI by Lancet Countdown Region designation and year.
aggregate(merged_data_2025$PopWeight_Avg_NDVI_2023,list(merged_data_2025$indicator_2023),mean)


#Average in 2023 
summary_table_2023a<-summary(data_all$peak.NDVI_2023)
summary_table_2023a<-summary_table_2023a[1:6]
summary_table_2023b<-summary(data_all$cumavg_2023)
summary_table_2023c<-summary(data_all$max_popweighted_2023)
summary_table_2023d<-summary(data_all$cum_popweighted_avg_2023)

summary_table_2023<-rbind(summary_table_2023a,summary_table_2023b,
                          summary_table_2023c,summary_table_2023d)




#Average in 2022 
summary_table_2022a<-summary(data_all$peak.NDVI_2022)
summary_table_2022a<-summary_table_2023a[1:6]
summary_table_2022b<-summary(data_all$cumavg_2022)
summary_table_2022c<-summary(data_all$max_popweighted_2022)
summary_table_2022d<-summary(data_all$cum_popweighted_avg_2022)

summary_table_2022<-rbind(summary_table_2022a,summary_table_2022b,
                          summary_table_2022c,summary_table_2022d)

write.csv(summary_table_2022, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/summary_table_2022.csv")


#Average in 2021 
summary_table_2021a<-summary(data_all$peak.NDVI_2021)
summary_table_2021a<-summary_table_2021a[1:6]
summary_table_2021b<-summary(data_all$cumavg_2021)
summary_table_2021b<-summary_table_2021b[1:6]
summary_table_2021c<-summary(data_all$max_popweighted_2021)
summary_table_2021d<-summary(data_all$cum_popweighted_avg_2021)

summary_table_2021<-rbind(summary_table_2021a,summary_table_2021b,
                          summary_table_2021c,summary_table_2021d)

write.csv(summary_table_2021, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2023/summary_table_2021.csv")


#Average in 2020 
summary_table_2020a<-summary(data_all$peak.NDVI_2020)
summary_table_2020a<-summary_table_2020a[1:6]
summary_table_2020b<-summary(data_all$cumavg_2020)
summary_table_2020c<-summary(data_all$max_popweighted_2020)
summary_table_2020d<-summary(data_all$cum_popweighted_avg_2020)

summary_table_2020<-rbind(summary_table_2020a,summary_table_2020b,
                          summary_table_2020c,summary_table_2020d)

write.csv(summary_table_2020, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/summary_table_2020.csv")

#Average in 2016 
summary_table_2016a<-summary(data_all$peak.NDVI_2016)
summary_table_2016a<-summary_table_2016a[1:6]
summary_table_2016b<-summary(data_all$cumavg_2016)
summary_table_2016c<-summary(data_all$max_popweighted_2016)
summary_table_2016d<-summary(data_all$cum_popweighted_avg_2016)

summary_table_2016<-rbind(summary_table_2016a,summary_table_2016b,
                          summary_table_2016c,summary_table_2016d)

write.csv(summary_table_2016, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/summary_table_2016.csv")

#Average in 2015 
summary_table_2015a<-summary(data_all$peak.NDVI_2015)
summary_table_2015a<-summary_table_2015a[1:6]
summary_table_2015b<-summary(data_all$cumavg_2015)
summary_table_2015c<-summary(data_all$max_popweighted_2015)
summary_table_2015d<-summary(data_all$cum_popweighted_avg_2015)

summary_table_2015<-rbind(summary_table_2015a,summary_table_2015b,
                          summary_table_2015c,summary_table_2015d)

write.csv(summary_table_2015, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/summary_table_2015.csv")


######################################
####### Create map for city capitals
######################################

# # Convert data frame to spatial

# #Create spatial points data frame
spdf <- SpatialPointsDataFrame(coords = data_all[, c(62,63)],
                               data = data_all,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

class(spdf) #[1] "SpatialPointsDataFrame" - attr(,"package") - "sp"

dataformaps1<-data_all[,c(2:13,59:68)]
dataformaps2<-data_all[,c(2:3,14:22,59:68)]
dataformaps3<-data_all[,c(2:3,23:31,59:68)]
dataformaps4<-data_all[,c(2:3,32:40,59:68)]
dataformaps5<-data_all[,c(2:3,41:49,59:68)]
dataformaps6<-data_all[,c(2:3,50:58,59:68)]

write.csv(dataformaps1,file="C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/dataformaps1.csv")
write.csv(dataformaps2,file="C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/dataformaps2.csv")
write.csv(dataformaps3,file="C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/dataformaps3.csv")
write.csv(dataformaps4,file="C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/dataformaps4.csv")
write.csv(dataformaps5,file="C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/dataformaps5.csv")
write.csv(dataformaps6,file="C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/dataformaps6.csv")



#############################
# Create Figures for report #
#############################
dataformaps1<-dataformaps1[order(dataformaps1$City),]
dataformaps1$X<-1:1041

dataformaps2<-dataformaps2[order(dataformaps2$City),]
dataformaps2$X<-1:1041

dataformaps3<-dataformaps3[order(dataformaps3$City),]
dataformaps3$X<-1:1041

dataformaps4<-dataformaps4[order(dataformaps4$City),]
dataformaps4$X<-1:1041

dataformaps5<-dataformaps5[order(dataformaps5$City),]
dataformaps5$X<-1:1041

dataformaps6<-dataformaps6[order(dataformaps5$City),]
dataformaps6$X<-1:1041

indicator_2015<-as.data.frame(table(data_all$indicator_2015))
names(indicator_2015)<-c("Value","Frequency")
indicator_2015<-indicator_2015[c(1,6,3,4,2,5),]

indicator_2016<-as.data.frame(table(data_all$indicator_2016))
names(indicator_2016)<-c("Value","Frequency")
indicator_2016<-indicator_2016[c(1,6,3,4,2,5),]

indicator_2020<-as.data.frame(table(data_all$indicator_2020))
names(indicator_2020)<-c("Value","Frequency")
indicator_2020<-indicator_2020[c(1,6,3,4,2,5),]

indicator_2021<-as.data.frame(table(data_all$indicator_2021))
names(indicator_2021)<-c("Value","Frequency")
indicator_2021<-indicator_2021[c(1,6,3,4,2,5),]

indicator_2022<-as.data.frame(table(data_all$indicator_2022))
names(indicator_2022)<-c("Value","Frequency")
indicator_2022<-indicator_2022[c(1,6,3,4,2,5),]

indicator_2023<-as.data.frame(table(data_all$indicator_2023))
names(indicator_2023)<-c("Value","Frequency")
indicator_2023<-indicator_2023[c(1,6,3,4,2,5),]


hdi_indicator_2015<-table(data_all$indicator_2015,data_all$hdi_level)
hdi_indicator_2016<-table(data_all$indicator_2016,data_all$hdi_level)
hdi_indicator_2020<-table(data_all$indicator_2020,data_all$hdi_level)
hdi_indicator_2021<-table(data_all$indicator_2021,data_all$hdi_level)
hdi_indicator_2022<-table(data_all$indicator_2022,data_all$hdi_level)
hdi_indicator_2023<-table(data_all$indicator_2023,data_all$hdi_level)

write.csv(hdi_indicator_2015,'C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/indicator_by_hdi_2015.csv')
write.csv(hdi_indicator_2016,'C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/indicator_by_hdi_2016.csv')
write.csv(hdi_indicator_2020,'C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/indicator_by_hdi_2020.csv')
write.csv(hdi_indicator_2021,'C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/indicator_by_hdi_2021.csv')
write.csv(hdi_indicator_2022,'C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/indicator_by_hdi_2022.csv')
write.csv(hdi_indicator_2023,'C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/indicator_by_hdi_2023.csv')

data_all_hdi_modgreen<-data_all %>%
  filter (indicator_2023 == "Moderate"|indicator_2023=="High"|
            indicator_2023=="Very High") 

data_all_hdi_modgreen_2023<-table(data_all_hdi_modgreen$indicator_2023,data_all_hdi_modgreen$hdi_level)
data_all_hdi_modgreen_2023<-as.data.frame(data_all_hdi_modgreen_2023)
aggregate(data_all_hdi_modgreen$max_popweighted_2023,by=list(data_all_hdi_modgreen$hdi_level), FUN='mean')
# Indicator   mean_max
# 1      High 0.4510756
# 2       Low 0.4752466
# 3    Medium 0.4685021
# 4 Very High 0.4535734

mean(data_all_hdi_modgreen$max_popweighted_2023)
#0.4603983


data_all_hdi_modgreen<-data_all %>%
  filter (indicator_2022 == "Moderate"|indicator_2022=="High"|
            indicator_2022=="Very High") 

data_all_hdi_modgreen_2022<-table(data_all_hdi_modgreen$indicator_2022,data_all_hdi_modgreen$hdi_level)
data_all_hdi_modgreen_2022<-as.data.frame(data_all_hdi_modgreen_2022)
aggregate(data_all_hdi_modgreen$max_popweighted_2022,by=list(data_all_hdi_modgreen$hdi_level), FUN='mean')
# Group.1         x
# 1      High 0.4527644
# 2       Low 0.4575150
# 3    Medium 0.4691185
# 4 Very High 0.4474336

mean(data_all_hdi_modgreen$max_popweighted_2022)
#0.4573981


data_all_hdi_modgreen<-data_all %>%
  filter (indicator_2021 == "Moderate"|indicator_2021=="High"|
            indicator_2021=="Very High") 

data_all_hdi_modgreen_2021<-table(data_all_hdi_modgreen$indicator_2021,data_all_hdi_modgreen$hdi_level)
data_all_hdi_modgreen_2021<-as.data.frame(data_all_hdi_modgreen_2021)
aggregate(data_all_hdi_modgreen$max_popweighted_2021,by=list(data_all_hdi_modgreen$hdi_level), FUN='mean')
# Group.1         x
# 1      High 0.4579408
# 2       Low 0.4692741
# 3    Medium 0.4705929
# 4 Very High 0.4514608

mean(data_all_hdi_modgreen$max_popweighted_2021)
# 0.4618128


data_all_hdi_modgreen<-data_all %>%
  filter (indicator_2020 == "Moderate"|indicator_2020=="High"|
            indicator_2020=="Very High") 

data_all_hdi_modgreen_2020<-table(data_all_hdi_modgreen$indicator_2020,data_all_hdi_modgreen$hdi_level)
data_all_hdi_modgreen_2020<-as.data.frame(data_all_hdi_modgreen_2020)
aggregate(data_all_hdi_modgreen$max_popweighted_2020,by=list(data_all_hdi_modgreen$hdi_level), FUN='mean')
# Group.1         x
# 1      High 0.4555620
# 2       Low 0.4600514
# 3    Medium 0.4773154
# 4 Very High 0.4503830

mean(data_all_hdi_modgreen$max_popweighted_2020)
# 0.4621691


data_all_hdi_modgreen<-data_all %>%
  filter (indicator_2016 == "Moderate"|indicator_2016=="High"|
            indicator_2016=="Very High") 

data_all_hdi_modgreen_2016<-table(data_all_hdi_modgreen$indicator_2016,data_all_hdi_modgreen$hdi_level)
data_all_hdi_modgreen_2016<-as.data.frame(data_all_hdi_modgreen_2016)
aggregate(data_all_hdi_modgreen$max_popweighted_2016,by=list(data_all_hdi_modgreen$hdi_level), FUN='mean')
# Group.1         x
# 1      High 0.4591698
# 2       Low 0.4241760
# 3    Medium 0.4691388
# 4 Very High 0.4523783

mean(data_all_hdi_modgreen$max_popweighted_2016)
# 0.4585084


data_all_hdi_modgreen<-data_all %>%
  filter (indicator_2015 == "Moderate"|indicator_2015=="High"|
            indicator_2015=="Very High") 

data_all_hdi_modgreen_2015<-table(data_all_hdi_modgreen$indicator_2015,data_all_hdi_modgreen$hdi_level)
data_all_hdi_modgreen_2015<-as.data.frame(data_all_hdi_modgreen_2015)
aggregate(data_all_hdi_modgreen$max_popweighted_2015,by=list(data_all_hdi_modgreen$hdi_level), FUN='mean')
# Group.1         x
# 1      High 0.4644891
# 2       Low 0.4795119
# 3    Medium 0.4713515
# 4 Very High 0.4462409

mean(data_all_hdi_modgreen$max_popweighted_2015)
# 0.4618684


table(data_all$indicator_2015,data_all$hdi_level)
# High Low Medium N/A Very High
# Exceptionally Low   37  19     15   2        16
# High                14   6     31   0        11
# Low                132  35    128   2        96
# Moderate            48  12     77   0        84
# Very High            1   1      2   0         0
# Very Low           141  32     44   3        47

table(data_all$indicator_2016,data_all$hdi_level)
# High Low Medium N/A Very High
# Exceptionally Low   46  32     31   3        23
# High                12   0     19   0        13
# Low                110  21    116   2        99
# Moderate            45   8     50   0        68
# Very Low           160  44     79   2        51

table(data_all$indicator_2020,data_all$hdi_level)
# High Low Medium N/A Very High
# Exceptionally Low   31  18     17   2        17
# High                11   6     31   0        12
# Low                149  32    126   2        91
# Moderate            50  13     74   0        84
# Very High            1   0      3   0         0
# Very Low           131  36     46   3        50

table(data_all$indicator_2021,data_all$hdi_level)
# High Low Medium N/A Very High
# Exceptionally Low   37  19     21   3        21
# High                11   5     35   0        11
# Low                141  33    111   4       103
# Moderate            49  11     83   0        73
# Very High            1   0      0   0         0
# Very Low           134  37     47   0        46

table(data_all$indicator_2022,data_all$hdi_level)
# High Low Medium N/A Very High
# Exceptionally Low   35  22     21   3        20
# High                11   2     28   0        12
# Low                144  31    127   3        95
# Moderate            55  12     78   0        79
# Very High            1   0      2   0         0
# Very Low           127  38     41   1        48

table(data_all$indicator_2023,data_all$hdi_level)
# High Low Medium N/A Very High
# Exceptionally Low   37  17     19   3        20
# High                 8   4     31   0        10
# Low                148  31    103   4       105
# Moderate            59  13     87   0        73
# Very High            1   1      4   0         0
# Very Low           120  39     51   0        46


table(data_all$indicator_2015,data_all$who_region)
# Africa Americas Eastern Mediterranean Europe South-East Asia
# Exceptionally Low      6       15                    59      1               0
# High                   7        8                     0      6              40
# Low                   47       68                    19     54             114
# Moderate              16       41                     6     64              77
# Very High              1        0                     0      0               3
# Very Low              35       46                    29     35              23
# 
# Western Pacific
# Exceptionally Low               8
# High                            1
# Low                            91
# Moderate                       17
# Very High                       0
# Very Low                       99

table(data_all$indicator_2016,data_all$who_region)
# Africa Americas Eastern Mediterranean Europe South-East Asia Western Pacific
# Exceptionally Low     19       24                    74      7               4               7
# High                   1        6                     0      9              28               0
# Low                   23       68                     9     53             114              81
# Moderate              12       27                     3     57              55              17
# Very Low              55       53                    27     34              56             111


table(data_all$indicator_2020,data_all$who_region)
# Africa Americas Eastern Mediterranean Europe South-East Asia
# Exceptionally Low     10       18                    52      2               0
# High                   7        8                     0      6              38
# Low                   42       67                    14     50             118
# Moderate              16       41                     7     66              75
# Very High              0        0                     0      0               4
# Very Low              35       44                    40     36              22
# 
# Western Pacific
# Exceptionally Low               2
# High                            1
# Low                           108
# Moderate                       16
# Very High                       0
# Very Low                       89

table(data_all$indicator_2021,data_all$who_region)
# Africa Americas Eastern Mediterranean Europe South-East Asia
# Exceptionally Low      9       20                    64      5               0
# High                   6        7                     0      8              40
# Low                   42       69                    13     57             106
# Moderate              14       38                     6     56              85
# Very High              0        0                     0      0               1
# Very Low              39       44                    30     34              25
# 
# Western Pacific
# Exceptionally Low               2
# High                            1
# Low                           104
# Moderate                       17
# Very High                       0
# Very Low                       92

table(data_all$indicator_2022,data_all$who_region)
# Africa Americas Eastern Mediterranean Europe South-East Asia
# Exceptionally Low     10       20                    64      5               0
# High                   3        9                     1      6              33
# Low                   43       69                    13     51             122
# Moderate              17       36                     3     65              80
# Very High              0        0                     0      0               3
# Very Low              37       44                    32     33              19
# 
# Western Pacific
# Exceptionally Low               1
# High                            1
# Low                           101
# Moderate                       23
# Very High                       0
# Very Low                       90

table(data_all$indicator_2023,data_all$who_region)
# Africa Americas Eastern Mediterranean Europe South-East Asia
# Exceptionally Low      7       20                    64      5               0
# High                   5        4                     1      8              32
# Low                   38       72                    16     53             105
# Moderate              20       40                     4     60              86
# Very High              1        0                     0      0               5
# Very Low              39       42                    28     34              29
# 
# Western Pacific
# Exceptionally Low               0
# High                            3
# Low                           107
# Moderate                       22
# Very High                       0
# Very Low                       84

table(data_all$indicator_2015,data_all$lc_group)
# Africa Asia Europe Northern America Oceania SIDS South and Central America
# Exceptionally Low     22   52      0                3       0    0                        12
# High                   6   41      6                8       0    1                         0
# Low                   53  219     44               14       3   15                        45
# Moderate              19   93     64               26       1    5                        13
# Very High              1    3      0                0       0    0                         0
# Very Low              49  156     14                6       2    1                        39


table(data_all$indicator_2016,data_all$lc_group)
# Africa Asia Europe Northern America Oceania SIDS South and Central America
# Exceptionally Low     42   67      2                4       0    0                        20
# High                   0   28      9                6       0    1                         0
# Low                   24  198     49               22       5   11                        39
# Moderate              14   70     57               20       0    5                         5
# Very Low              68  201     11                5       1    5                        45


table(data_all$indicator_2020,data_all$lc_group)
# Africa Asia Europe Northern America Oceania SIDS South and Central America
# Exceptionally Low     25   40      1                3       0    0                        15
# High                   6   39      6                8       0    1                         0
# Low                   45  233     44               15       5   11                        46
# Moderate              18   92     66               25       0    8                        12
# Very High              0    4      0                0       0    0                         0
# Very Low              54  156     11                6       1    2                        36

table(data_all$indicator_2021,data_all$lc_group)
# Africa Asia Europe Northern America Oceania SIDS South and Central America
# Exceptionally Low     25   52      3                4       0    0                        16
# High                   5   41      8                6       0    1                         1
# Low                   47  216     51               20       5   10                        42
# Moderate              17   99     56               22       0    9                        13
# Very High              0    1      0                0       0    0                         0
# Very Low              54  155     10                5       1    2                        37

table(data_all$indicator_2022,data_all$lc_group)
# Africa Asia Europe Northern America Oceania SIDS South and Central America
# Exceptionally Low     25   52      3                3       0    0                        17
# High                   3   34      6                9       0    1                         0
# Low                   48  231     45               19       3   10                        43
# Moderate              18   98     65               20       1   10                        12
# Very High              0    3      0                0       0    0                         0
# Very Low              54  146      9                6       2    1                        37

table(data_all$indicator_2023,data_all$lc_group)
# Africa Asia Europe Northern America Oceania SIDS South and Central America
# Exceptionally Low     24   49      3                4       0    0                        16
# High                   5   34      8                4       0    2                         0
# Low                   44  220     47               22       5    8                        45
# Moderate              21  106     60               22       0   10                        13
# Very High              1    5      0                0       0    0                         0
# Very Low              53  150     10                5       1    2                        35

table(data_all$indicator_2015,data_all$clim_region)
# Arid Continental Polar Temperate Tropical
# Exceptionally Low   75           3     1         6        1
# High                 0          10     0        13       36
# Low                 52          39     0       162      116
# Moderate             9          57     0        87       67
# Very High            0           0     0         0        4
# Very Low            86          31     0        90       48

table(data_all$indicator_2016,data_all$clim_region)
# Arid Continental Polar Temperate Tropical
# Exceptionally Low  102           2     1        14       12
# High                 1           7     0         9       25
# Low                 26          49     0       154      104
# Moderate             5          51     0        59       55
# Very Low            88          31     0       122       76

table(data_all$indicator_2020,data_all$clim_region)
# Arid Continental Polar Temperate Tropical
# Exceptionally Low   71           0     1         6        4
# High                 0           8     0        15       34
# Low                 51          53     0       158      112
# Moderate            11          54     0        81       76
# Very High            0           0     0         1        3
# Very Low            89          25     0        97       43

table(data_all$indicator_2021,data_all$clim_region)
# Arid Continental Polar Temperate Tropical
# Exceptionally Low   80           3     1         9        4
# High                 0           9     0        14       37
# Low                 42          55     0       169      105
# Moderate            13          51     0        78       72
# Very High            0           0     0         0        1
# Very Low            87          22     0        88       53

table(data_all$indicator_2022,data_all$clim_region)
# Arid Continental Polar Temperate Tropical
# Exceptionally Low   80           0     1        11        4
# High                 1          11     0        11       28
# Low                 50          40     0       179      110
# Moderate            10          61     0        75       77
# Very High            0           0     0         0        3
# Very Low            81          28     0        82       50

table(data_all$indicator_2023,data_all$clim_region)
# Arid Continental Polar Temperate Tropical
# Exceptionally Low   80           1     1         8        3
# High                 1           6     0        17       27
# Low                 47          60     0       155      104
# Moderate             8          54     0        89       79
# Very High            0           0     0         1        5
# Very Low            86          19     0        88       54



# data_all_africa<-subset(data_all,who_region=="Africa")
# data_all_americas<-subset(data_all,who_region=="Americas")
# data_all_e_medit<-subset(data_all,who_region=="Eastern Mediterranean")
# data_all_european<-subset(data_all,who_region=="Europe")
# data_all_se_asian<-subset(data_all,who_region=="South-East Asia")
# data_all_w_pacific<-subset(data_all,who_region=="Western Pacific")
# 
# summary(data_all_africa$peak.NDVI_2021)
# summary(data_all_africa$cumavg_2021)
# summary(data_all_africa$max_popweighted_2021)
# summary(data_all_africa$cum_popweighted_avg_2021)
# 
# summary(data_all_americas$peak.NDVI_2021)
# summary(data_all_americas$cumavg_2021)
# summary(data_all_americas$max_popweighted_2021)
# summary(data_all_americas$cum_popweighted_avg_2021)
# 
# summary(data_all_e_medit$peak.NDVI_2021)
# summary(data_all_e_medit$cumavg_2021)
# summary(data_all_e_medit$max_popweighted_2021)
# summary(data_all_e_medit$cum_popweighted_avg_2021)
# 
# summary(data_all_european$peak.NDVI_2021)
# summary(data_all_european$cumavg_2021)
# summary(data_all_european$max_popweighted_2021)
# summary(data_all_european$cum_popweighted_avg_2021)
# 
# summary(data_all_se_asian$peak.NDVI_2021)
# summary(data_all_se_asian$cumavg_2021)
# summary(data_all_se_asian$max_popweighted_2021)
# summary(data_all_se_asian$cum_popweighted_avg_2021)
# 
# summary(data_all_w_pacific$peak.NDVI_2021)
# summary(data_all_w_pacific$cumavg_2021)
# summary(data_all_w_pacific$max_popweighted_2021)
# summary(data_all_w_pacific$cum_popweighted_avg_2021)

data_all_arid<-subset(data_all,clim_region=="Arid")
data_all_cont<-subset(data_all,clim_region=="Continental")
data_all_polar<-subset(data_all,clim_region=="Polar")
data_all_temper<-subset(data_all,clim_region=="Temperate")
data_all_trop<-subset(data_all,clim_region=="Tropical")

sum_arid_2015<-summary(data_all_arid$max_popweighted_2015)
sum_cont_2015<-summary(data_all_cont$max_popweighted_2015)
sum_polar_2015<-summary(data_all_polar$max_popweighted_2015)
sum_temper_2015<-summary(data_all_temper$max_popweighted_2015)
sum_trop_2015<-summary(data_all_trop$max_popweighted_2015)

summary_climreg_2015<-rbind(sum_arid_2015,sum_cont_2015,sum_polar_2015,
                            sum_temper_2015,sum_trop_2015)

write.csv(summary_climreg_2015, 
          "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/summary_climreg_2015.csv")

sum_arid_2016<-summary(data_all_arid$max_popweighted_2016)
sum_cont_2016<-summary(data_all_cont$max_popweighted_2016)
sum_polar_2016<-summary(data_all_polar$max_popweighted_2016)
sum_temper_2016<-summary(data_all_temper$max_popweighted_2016)
sum_trop_2016<-summary(data_all_trop$max_popweighted_2016)

summary_climreg_2016<-rbind(sum_arid_2016,sum_cont_2016,sum_polar_2016,
                            sum_temper_2016,sum_trop_2016)

write.csv(summary_climreg_2016, 
          "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/summary_climreg_2016.csv")


sum_arid_2020<-summary(data_all_arid$max_popweighted_2020)
sum_cont_2020<-summary(data_all_cont$max_popweighted_2020)
sum_polar_2020<-summary(data_all_polar$max_popweighted_2020)
sum_temper_2020<-summary(data_all_temper$max_popweighted_2020)
sum_trop_2020<-summary(data_all_trop$max_popweighted_2020)

summary_climreg_2020<-rbind(sum_arid_2020,sum_cont_2020,sum_polar_2020,
                            sum_temper_2020,sum_trop_2020)

write.csv(summary_climreg_2020, 
          "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/summary_climreg_2020.csv")


sum_arid_2021<-summary(data_all_arid$max_popweighted_2021)
sum_cont_2021<-summary(data_all_cont$max_popweighted_2021)
sum_polar_2021<-summary(data_all_polar$max_popweighted_2021)
sum_temper_2021<-summary(data_all_temper$max_popweighted_2021)
sum_trop_2021<-summary(data_all_trop$max_popweighted_2021)

summary_climreg_2021<-rbind(sum_arid_2021,sum_cont_2021,sum_polar_2021,
                            sum_temper_2021,sum_trop_2021)

write.csv(summary_climreg_2021, 
          "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/summary_climreg_2021.csv")

sum_arid_2022<-summary(data_all_arid$max_popweighted_2022)
sum_cont_2022<-summary(data_all_cont$max_popweighted_2022)
sum_polar_2022<-summary(data_all_polar$max_popweighted_2022)
sum_temper_2022<-summary(data_all_temper$max_popweighted_2022)
sum_trop_2022<-summary(data_all_trop$max_popweighted_2022)

summary_climreg_2022<-rbind(sum_arid_2022,sum_cont_2022,sum_polar_2022,
                            sum_temper_2022,sum_trop_2022)

write.csv(summary_climreg_2022, 
          "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/summary_climreg_2022.csv")

sum_arid_2023<-summary(data_all_arid$max_popweighted_2023)
sum_cont_2023<-summary(data_all_cont$max_popweighted_2023)
sum_polar_2023<-summary(data_all_polar$max_popweighted_2023)
sum_temper_2023<-summary(data_all_temper$max_popweighted_2023)
sum_trop_2023<-summary(data_all_trop$max_popweighted_2023)

summary_climreg_2023<-rbind(sum_arid_2023,sum_cont_2023,sum_polar_2023,
                            sum_temper_2023,sum_trop_2023)

write.csv(summary_climreg_2023, 
          "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/summary_climreg_2023.csv")


###########################
#### Greenness Tables #####
###########################

#2015
a<-tapply(data_all$peak.NDVI_2015,data_all$indicator_2015, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2015,data_all$indicator_2015, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2015,data_all$indicator_2015, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2015, data_all$indicator_2015, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2015,na.rm=TRUE)
e2<-mean(data_all$cumavg_2015,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2015,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2015,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table2<-rbind(a,b,c,d)
table2<-t(table2)
table2<-rbind(table2,e)
table2<-round(table2,2)
table2<-as.data.frame(table2)
names(table2) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table2$year<-rep('2015',7)


#2020
data_all$peak.NDVI_2020<-as.numeric(data_all$peak.NDVI_2020)
a<-tapply(data_all$peak.NDVI_2020,data_all$indicator_2020, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2020,data_all$indicator_2020, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2020,data_all$indicator_2020, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2020, data_all$indicator_2020, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2020,na.rm=TRUE)
e2<-mean(data_all$cumavg_2020,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2020,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2020,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table4<-rbind(a,b,c,d)
table4<-t(table4)
table4<-rbind(table4,e)
table4<-round(table4,2)
table4<-as.data.frame(table4)
names(table4) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table4$year<-rep('2020',7)

#2021
data_all$peak.NDVI_2021<-as.numeric(data_all$peak.NDVI_2021)
a<-tapply(data_all$peak.NDVI_2021,data_all$indicator_2021, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2021,data_all$indicator_2021, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2021,data_all$indicator_2021, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2021, data_all$indicator_2021, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2021,na.rm=TRUE)
e2<-mean(data_all$cumavg_2021,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2021,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2021,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table5<-rbind(a,b,c,d)
table5<-t(table5)
table5<-rbind(table5,e)
table5<-round(table5,2)
table5<-as.data.frame(table5)
names(table5) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table5$year<-rep('2021',7)


#2022
data_all$peak.NDVI_2022<-as.numeric(data_all$peak.NDVI_2022)
a<-tapply(data_all$peak.NDVI_2022,data_all$indicator_2022, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2022,data_all$indicator_2022, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2022,data_all$indicator_2022, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2022, data_all$indicator_2022, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2022,na.rm=TRUE)
e2<-mean(data_all$cumavg_2022,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2022,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2022,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table6<-rbind(a,b,c,d)
table6<-t(table6)
table6<-rbind(table6,e)
table6<-round(table6,2)
table6<-as.data.frame(table6)
names(table6) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table6$year<-rep('2022',7)

#2023
data_all$peak.NDVI_2023<-as.numeric(data_all$peak.NDVI_2023)
a<-tapply(data_all$peak.NDVI_2023,data_all$indicator_2023, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2023,data_all$indicator_2023, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2023,data_all$indicator_2023, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2023, data_all$indicator_2023, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2023,na.rm=TRUE)
e2<-mean(data_all$cumavg_2023,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2023,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2023,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table7<-rbind(a,b,c,d)
table7<-t(table7)
table7<-rbind(table7,e)
table7<-round(table7,2)
table7<-as.data.frame(table7)
names(table7) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table7$year<-rep('2023',7)

table1<-rbind(table2,table3,table4,table5,table6,table7)

write.csv(table1, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/table1_ndvi_level.csv")
write.csv(data_all, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/data_all.csv")


#####################
#### HDI Tables #####
#####################

hdi_green_2015<-table(data_all$indicator_2015,data_all$hdi_level)
hdi_green_2015<-as.data.frame(hdi_green_2015)
aggregate(data_all$max_popweighted_2015,by=list(data_all$hdi_level), FUN='mean')
# Group.1         x
# 1      High 0.3127305
# 2       Low 0.3053310
# 3    Medium 0.3718103
# 4       N/A 0.2521114
# 5 Very High 0.3573224
mean(data_all$max_popweighted_2015)
# 0.3392995

hdi_green_2016<-table(data_all$indicator_2016,data_all$hdi_level)
hdi_green_2016<-as.data.frame(hdi_green_2016)
aggregate(data_all$max_popweighted_2016,by=list(data_all$hdi_level), FUN='mean')
# Group.1         x
# 1      High 0.3002258
# 2       Low 0.2497418
# 3    Medium 0.3321110
# 4       N/A 0.2523373
# 5 Very High 0.3460824
mean(data_all$max_popweighted_2016)
#0.3149999

hdi_green_2020<-table(data_all$indicator_2020,data_all$hdi_level)
hdi_green_2020<-as.data.frame(hdi_green_2020)
aggregate(data_all$max_popweighted_2020,by=list(data_all$hdi_level), FUN='mean')
# Group.1         x
# 1      High 0.3174538
# 2       Low 0.2991183
# 3    Medium 0.3720231
# 4       N/A 0.2481288
# 5 Very High 0.3565817
mean(data_all$max_popweighted_2020)
# 0.3402021

hdi_green_2021<-table(data_all$indicator_2021,data_all$hdi_level)
hdi_green_2021<-as.data.frame(hdi_green_2021)
aggregate(data_all$max_popweighted_2021,by=list(data_all$hdi_level), FUN='mean')
# Group.1         x
# 1      High 0.3135127
# 2       Low 0.2946950
# 3    Medium 0.3696600
# 4       N/A 0.2734192
# 5 Very High 0.3514757
mean(data_all$max_popweighted_2021)
# 0.3365776

hdi_green_2022<-table(data_all$indicator_2022,data_all$hdi_level)
hdi_green_2022<-as.data.frame(hdi_green_2022)
aggregate(data_all$max_popweighted_2022,by=list(data_all$hdi_level), FUN='mean')
# Group.1         x
# 1      High 0.3161222
# 2       Low 0.2852035
# 3    Medium 0.3671457
# 4       N/A 0.2596532
# 5 Very High 0.3507091
mean(data_all$max_popweighted_2022)
# 0.335592

hdi_green_2023<-table(data_all$indicator_2023,data_all$hdi_level)
hdi_green_2023<-as.data.frame(hdi_green_2023)
aggregate(data_all$max_popweighted_2023,by=list(data_all$hdi_level), FUN='mean')
# Group.1         x
# 1      High 0.3180441
# 2       Low 0.3030738
# 3    Medium 0.3693712
# 4       N/A 0.2518843
# 5 Very High 0.3522699
mean(data_all$max_popweighted_2023)
# 0.3389941


#2015
a<-tapply(data_all$peak.NDVI_2015,data_all$hdi_level, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2015,data_all$hdi_level, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2015,data_all$hdi_level, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2015, data_all$hdi_level, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2015,na.rm=TRUE)
e2<-mean(data_all$cumavg_2015,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2015,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2015,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table2<-rbind(a,b,c,d)
table2<-t(table2)
table2<-rbind(table2,e)
table2<-round(table2,2)
table2<-as.data.frame(table2)
names(table2) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table2$year<-rep('2015',6)

#2016
a<-tapply(data_all$peak.NDVI_2016,data_all$hdi_level, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2016,data_all$hdi_level, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2016,data_all$hdi_level, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2016, data_all$hdi_level, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2016,na.rm=TRUE)
e2<-mean(data_all$cumavg_2016,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2016,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2016,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table3<-rbind(a,b,c,d)
table3<-t(table3)
table3<-rbind(table3,e)
table3<-round(table3,2)
table3<-as.data.frame(table3)
names(table3) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table3$year<-rep('2016',6)

#2020
data_all$peak.NDVI_2020<-as.numeric(data_all$peak.NDVI_2020)
a<-tapply(data_all$peak.NDVI_2020,data_all$hdi_level, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2020,data_all$hdi_level, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2020,data_all$hdi_level, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2020, data_all$hdi_level, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2020,na.rm=TRUE)
e2<-mean(data_all$cumavg_2020,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2020,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2020,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table4<-rbind(a,b,c,d)
table4<-t(table4)
table4<-rbind(table4,e)
table4<-round(table4,2)
table4<-as.data.frame(table4)
names(table4) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table4$year<-rep('2020',6)

#2021
data_all$peak.NDVI_2021<-as.numeric(data_all$peak.NDVI_2021)
a<-tapply(data_all$peak.NDVI_2021,data_all$hdi_level, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2021,data_all$hdi_level, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2021,data_all$hdi_level, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2021, data_all$hdi_level, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2021,na.rm=TRUE)
e2<-mean(data_all$cumavg_2021,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2021,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2021,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table5<-rbind(a,b,c,d)
table5<-t(table5)
table5<-rbind(table5,e)
table5<-round(table5,2)
table5<-as.data.frame(table5)
names(table5) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table5$year<-rep('2021',6)

#2022
data_all$peak.NDVI_2022<-as.numeric(data_all$peak.NDVI_2022)
a<-tapply(data_all$peak.NDVI_2022,data_all$hdi_level, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2022,data_all$hdi_level, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2022,data_all$hdi_level, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2022, data_all$hdi_level, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2022,na.rm=TRUE)
e2<-mean(data_all$cumavg_2022,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2022,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2022,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table6<-rbind(a,b,c,d)
table6<-t(table6)
table6<-rbind(table6,e)
table6<-round(table6,2)
table6<-as.data.frame(table6)
names(table6) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table6$year<-rep('2022',6)

#2023
data_all$peak.NDVI_2023<-as.numeric(data_all$peak.NDVI_2023)
a<-tapply(data_all$peak.NDVI_2023,data_all$hdi_level, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2023,data_all$hdi_level, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2023,data_all$hdi_level, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2023, data_all$hdi_level, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2023,na.rm=TRUE)
e2<-mean(data_all$cumavg_2023,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2023,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2023,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table7<-rbind(a,b,c,d)
table7<-t(table7)
table7<-rbind(table7,e)
table7<-round(table7,2)
table7<-as.data.frame(table7)
names(table7) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table7$year<-rep('2023',6)

table1<-rbind(table2,table3,table4,table5,table6,table7)

write.csv(table1, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/table1_hdilevel.csv")




#############################
#### Clim Region Tables #####
#############################
data_all_clim_modgreen<-data_all %>%
  filter (indicator_2023 == "Moderate"|indicator_2023=="High"|
            indicator_2023=="Very High") 

data_all_clim_modgreen_2023<-table(data_all_clim_modgreen$indicator_2023,data_all_clim_modgreen$clim_region)
data_all_clim_modgreen_2023<-as.data.frame(data_all_clim_modgreen_2023)

data_all_clim_modgreen<-data_all %>%
  filter (indicator_2022 == "Moderate"|indicator_2022=="High"|
            indicator_2022=="Very High") 

data_all_clim_modgreen_2022<-table(data_all_clim_modgreen$indicator_2022,data_all_clim_modgreen$clim_region)
data_all_clim_modgreen_2022<-as.data.frame(data_all_clim_modgreen_2022)

data_all_clim_modgreen<-data_all %>%
  filter (indicator_2021 == "Moderate"|indicator_2021=="High"|
            indicator_2021=="Very High") 

data_all_clim_modgreen_2021<-table(data_all_clim_modgreen$indicator_2021,data_all_clim_modgreen$clim_region)
data_all_clim_modgreen_2021<-as.data.frame(data_all_clim_modgreen_2021)

data_all_clim_modgreen<-data_all %>%
  filter (indicator_2020 == "Moderate"|indicator_2020=="High"|
            indicator_2020=="Very High") 

data_all_clim_modgreen_2020<-table(data_all_clim_modgreen$indicator_2020,data_all_clim_modgreen$clim_region)
data_all_clim_modgreen_2020<-as.data.frame(data_all_clim_modgreen_2020)

data_all_clim_modgreen<-data_all %>%
  filter (indicator_2016 == "Moderate"|indicator_2016=="High"|
            indicator_2016=="Very High") 

data_all_clim_modgreen_2016<-table(data_all_clim_modgreen$indicator_2016,data_all_clim_modgreen$clim_region)
data_all_clim_modgreen_2016<-as.data.frame(data_all_clim_modgreen_2016)

data_all_clim_modgreen<-data_all %>%
  filter (indicator_2015 == "Moderate"|indicator_2015=="High"|
            indicator_2015=="Very High") 

data_all_clim_modgreen_2015<-table(data_all_clim_modgreen$indicator_2015,data_all_clim_modgreen$clim_region)
data_all_clim_modgreen_2015<-as.data.frame(data_all_clim_modgreen_2015)

sum_climreg_2015<-table(data_all$indicator_2015,data_all$clim_region)
sum_climreg_2015<-as.data.frame(sum_climreg_2015)
aggregate(data_all$max_popweighted_2015,by=list(data_all$clim_region), FUN='mean')
mean(data_all$max_popweighted_2015)
# Group.1         x
# 1        Arid 0.2421333
# 2 Continental 0.3730018
# 3       Polar 0.1402273
# 4   Temperate 0.3532709
# 5    Tropical 0.3869281
# 0.3392995

sum_climreg_2016<-table(data_all$indicator_2016,data_all$clim_region)
sum_climreg_2016<-as.data.frame(sum_climreg_2016)
aggregate(data_all$max_popweighted_2016,by=list(data_all$clim_region), FUN='mean')
mean(data_all$max_popweighted_2016)
# Group.1         x
# 1        Arid 0.2154322
# 2 Continental 0.3749328
# 3       Polar 0.1310032
# 4   Temperate 0.3284686
# 5    Tropical 0.3516362
# 0.3149999

sum_climreg_2020<-table(data_all$indicator_2020,data_all$clim_region)
sum_climreg_2020<-as.data.frame(sum_climreg_2020)
aggregate(data_all$max_popweighted_2020,by=list(data_all$clim_region), FUN='mean')
# Group.1         x
# 1        Arid 0.2459148
# 2 Continental 0.3853420
# 3       Polar 0.1264839
# 4   Temperate 0.3509529
# 5    Tropical 0.3849895
mean(data_all$max_popweighted_2020)
#0.3402021

sum_climreg_2021<-table(data_all$indicator_2021,data_all$clim_region)
sum_climreg_2021<-as.data.frame(sum_climreg_2021)
aggregate(data_all$max_popweighted_2021,by=list(data_all$clim_region), FUN='mean')
# Group.1         x
# 1        Arid 0.2399964
# 2 Continental 0.3776325
# 3       Polar 0.1300610
# 4   Temperate 0.3513142
# 5    Tropical 0.3799616
mean(data_all$max_popweighted_2021)
# 0.3365776

sum_climreg_2022<-table(data_all$indicator_2022,data_all$clim_region)
sum_climreg_2022<-as.data.frame(sum_climreg_2022)
aggregate(data_all$max_popweighted_2022,by=list(data_all$clim_region), FUN='mean')
# Group.1         x
# 1        Arid 0.2389636
# 2 Continental 0.3840083
# 3       Polar 0.1382738
# 4   Temperate 0.3477812
# 5    Tropical 0.3789136
mean(data_all$max_popweighted_2022)
#0.335592

sum_climreg_2023<-table(data_all$indicator_2023,data_all$clim_region)
sum_climreg_2023<-as.data.frame(sum_climreg_2023)
aggregate(data_all$max_popweighted_2023,by=list(data_all$clim_region), FUN='mean')
# Group.1         x
# 1        Arid 0.2393015
# 2 Continental 0.3853281
# 3       Polar 0.1119470
# 4   Temperate 0.3548141
# 5    Tropical 0.3800856
mean(data_all$max_popweighted_2023)
#0.3389941


#2015
a<-tapply(data_all$peak.NDVI_2015,data_all$clim_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2015,data_all$clim_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2015,data_all$clim_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2015, data_all$clim_region, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2015,na.rm=TRUE)
e2<-mean(data_all$cumavg_2015,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2015,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2015,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table2<-rbind(a,b,c,d)
table2<-t(table2)
table2<-rbind(table2,e)
table2<-round(table2,2)
table2<-as.data.frame(table2)
names(table2) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table2$year<-rep('2015',6)


#2016
a<-tapply(data_all$peak.NDVI_2016,data_all$clim_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2016,data_all$clim_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2016,data_all$clim_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2016, data_all$clim_region, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2016,na.rm=TRUE)
e2<-mean(data_all$cumavg_2016,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2016,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2016,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table3<-rbind(a,b,c,d)
table3<-t(table3)
table3<-rbind(table3,e)
table3<-round(table3,2)
table3<-as.data.frame(table3)
names(table3) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table3$year<-rep('2016',6)


#2020
data_all$peak.NDVI_2020<-as.numeric(data_all$peak.NDVI_2020)
a<-tapply(data_all$peak.NDVI_2020,data_all$clim_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2020,data_all$clim_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2020,data_all$clim_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2020, data_all$clim_region, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2020,na.rm=TRUE)
e2<-mean(data_all$cumavg_2020,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2020,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2020,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table4<-rbind(a,b,c,d)
table4<-t(table4)
table4<-rbind(table4,e)
table4<-round(table4,2)
table4<-as.data.frame(table4)
names(table4) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table4$year<-rep('2020',6)

#2021
data_all$peak.NDVI_2021<-as.numeric(data_all$peak.NDVI_2021)
a<-tapply(data_all$peak.NDVI_2021,data_all$clim_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2021,data_all$clim_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2021,data_all$clim_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2021, data_all$clim_region, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2021,na.rm=TRUE)
e2<-mean(data_all$cumavg_2021,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2021,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2021,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table5<-rbind(a,b,c,d)
table5<-t(table5)
table5<-rbind(table5,e)
table5<-round(table5,2)
table5<-as.data.frame(table5)
names(table5) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table5$year<-rep('2021',6)

#2022
data_all$peak.NDVI_2022<-as.numeric(data_all$peak.NDVI_2022)
a<-tapply(data_all$peak.NDVI_2022,data_all$clim_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2022,data_all$clim_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2022,data_all$clim_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2022, data_all$clim_region, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2022,na.rm=TRUE)
e2<-mean(data_all$cumavg_2022,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2022,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2022,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table6<-rbind(a,b,c,d)
table6<-t(table6)
table6<-rbind(table6,e)
table6<-round(table6,2)
table6<-as.data.frame(table6)
names(table6) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table6$year<-rep('2022',6)


#2023
data_all$peak.NDVI_2023<-as.numeric(data_all$peak.NDVI_2023)
a<-tapply(data_all$peak.NDVI_2023,data_all$clim_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2023,data_all$clim_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2023,data_all$clim_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2023, data_all$clim_region, mean, na.rm=TRUE)

e1<-mean(data_all$peak.NDVI_2023,na.rm=TRUE)
e2<-mean(data_all$cumavg_2023,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2023,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2023,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table7<-rbind(a,b,c,d)
table7<-t(table7)
table7<-rbind(table7,e)
table7<-round(table7,2)
table7<-as.data.frame(table7)
names(table7) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table7$year<-rep('2023',6)


table1<-rbind(table2,table3,table4,table5,table6,table7)

write.csv(table1, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/table1_climregion.csv")



####################
#### WHO Tables ####
####################

sum_who_2015<-table(data_all$indicator_2015,data_all$who_region)
sum_who_2015<-as.data.frame(sum_who_2015)
aggregate(data_all$max_popweighted_2015,by=list(data_all$who_region), FUN='mean')
# Group.1         x
# 1                Africa 0.3349681
# 2              Americas 0.3368855
# 3 Eastern Mediterranean 0.2160719
# 4                Europe 0.3749724
# 5       South-East Asia 0.4041587
# 6       Western Pacific 0.3050835
mean(data_all$max_popweighted_2015)
#0.3392995

sum_who_2016<-table(data_all$indicator_2016,data_all$who_region)
sum_who_2016<-as.data.frame(sum_who_2016)
aggregate(data_all$max_popweighted_2016,by=list(data_all$who_region), FUN='mean')
# Group.1         x
# 1                Africa 0.2735900
# 2              Americas 0.3148949
# 3 Eastern Mediterranean 0.1865267
# 4                Europe 0.3692743
# 5       South-East Asia 0.3688452
# 6       Western Pacific 0.2997701
mean(data_all$max_popweighted_2016)
#0.3149999

sum_who_2020<-table(data_all$indicator_2020,data_all$who_region)
sum_who_2020<-as.data.frame(sum_who_2020)
aggregate(data_all$max_popweighted_2020,by=list(data_all$who_region), FUN='mean')
# Group.1         x
# 1                Africa 0.3228180
# 2              Americas 0.3356433
# 3 Eastern Mediterranean 0.2177327
# 4                Europe 0.3746653
# 5       South-East Asia 0.4036509
# 6       Western Pacific 0.3167989

mean(data_all$max_popweighted_2020)
#0.3402021

sum_who_2021<-table(data_all$indicator_2021,data_all$who_region)
sum_who_2021<-as.data.frame(sum_who_2021)
aggregate(data_all$max_popweighted_2021,by=list(data_all$who_region), FUN='mean')
# Group.1         x
# 1                Africa 0.3145374
# 2              Americas 0.3290808
# 3 Eastern Mediterranean 0.2078769
# 4                Europe 0.3717886
# 5       South-East Asia 0.4052931
# 6       Western Pacific 0.3144436
mean(data_all$max_popweighted_2021)
#0.3365776

sum_who_2022<-table(data_all$indicator_2022,data_all$who_region)
sum_who_2022<-as.data.frame(sum_who_2022)
aggregate(data_all$max_popweighted_2022,by=list(data_all$who_region), FUN='mean')
# Group.1         x
# 1                Africa 0.3128385
# 2              Americas 0.3310736
# 3 Eastern Mediterranean 0.2032413
# 4                Europe 0.3684955
# 5       South-East Asia 0.4014130
# 6       Western Pacific 0.3182511
mean(data_all$max_popweighted_2022)
#0.335592

sum_who_2023<-table(data_all$indicator_2023,data_all$who_region)
sum_who_2023<-as.data.frame(sum_who_2023)
aggregate(data_all$max_popweighted_2023,by=list(data_all$who_region), FUN='mean')
# Group.1         x
# 1                Africa 0.3278642
# 2              Americas 0.3118223
# 3 Eastern Mediterranean 0.2172247
# 4                Europe 0.3693615
# 5       South-East Asia 0.3998594
# 6       Western Pacific 0.3241089
mean(data_all$max_popweighted_2023)
#0.339832


#2015
data_all$peak.NDVI_2015<-as.numeric(data_all$peak.NDVI_2015)
a<-tapply(data_all$peak.NDVI_2015,data_all$who_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2015,data_all$who_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2015,data_all$who_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2015, data_all$who_region, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2015,na.rm=TRUE)
e2<-mean(data_all$cumavg_2015,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2015,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2015,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table2<-rbind(a,b,c,d)
table2<-t(table2)
table2<-rbind(table2,e)
table2<-round(table2,2)
table2<-as.data.frame(table2)
names(table2) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table2$year<-rep('2015',7)

#2016
data_all$peak.NDVI_2016<-as.numeric(data_all$peak.NDVI_2016)
a<-tapply(data_all$peak.NDVI_2016,data_all$who_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2016,data_all$who_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2016,data_all$who_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2016, data_all$who_region, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2016,na.rm=TRUE)
e2<-mean(data_all$cumavg_2016,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2016,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2016,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table3<-rbind(a,b,c,d)
table3<-t(table3)
table3<-rbind(table3,e)
table3<-round(table3,2)
table3<-as.data.frame(table3)
names(table3) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table3$year<-rep('2016',7)

#2020
data_all$peak.NDVI_2020<-as.numeric(data_all$peak.NDVI_2020)
a<-tapply(data_all$peak.NDVI_2020,data_all$who_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2020,data_all$who_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2020,data_all$who_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2020, data_all$who_region, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2020,na.rm=TRUE)
e2<-mean(data_all$cumavg_2020,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2020,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2020,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table4<-rbind(a,b,c,d)
table4<-t(table4)
table4<-rbind(table4,e)
table4<-round(table4,2)
table4<-as.data.frame(table4)
names(table4) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table4$year<-rep('2020',7)

#2021
data_all$peak.NDVI_2021<-as.numeric(data_all$peak.NDVI_2021)
a<-tapply(data_all$peak.NDVI_2021,data_all$who_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2021,data_all$who_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2021,data_all$who_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2021, data_all$who_region, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2021,na.rm=TRUE)
e2<-mean(data_all$cumavg_2021,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2021,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2021,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table5<-rbind(a,b,c,d)
table5<-t(table5)
table5<-rbind(table5,e)
table5<-round(table5,2)
table5<-as.data.frame(table5)
names(table5) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table5$year<-rep('2021',7)

#2022
data_all$peak.NDVI_2022<-as.numeric(data_all$peak.NDVI_2022)
a<-tapply(data_all$peak.NDVI_2022,data_all$who_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2022,data_all$who_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2022,data_all$who_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2022, data_all$who_region, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2022,na.rm=TRUE)
e2<-mean(data_all$cumavg_2022,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2022,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2022,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table6<-rbind(a,b,c,d)
table6<-t(table6)
table6<-rbind(table6,e)
table6<-round(table6,2)
table6<-as.data.frame(table6)
names(table6) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table6$year<-rep('2022',7)

#2023
data_all$peak.NDVI_2023<-as.numeric(data_all$peak.NDVI_2023)
a<-tapply(data_all$peak.NDVI_2023,data_all$who_region, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2023,data_all$who_region, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2023,data_all$who_region, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2023, data_all$who_region, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2023,na.rm=TRUE)
e2<-mean(data_all$cumavg_2023,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2023,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2023,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table7<-rbind(a,b,c,d)
table7<-t(table7)
table7<-rbind(table7,e)
table7<-round(table7,2)
table7<-as.data.frame(table7)
names(table7) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table7$year<-rep('2023',7)


table1<-rbind(table2,table3,table4,table5,table6,table7)

write.csv(table1, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/table1_who.csv")


####################
#### LCD Tables ####
####################

sum_lcd_2015<-table(data_all$indicator_2015,data_all$lc_group)
sum_lcd_2015<-as.data.frame(sum_lcd_2015)
aggregate(data_all$max_popweighted_2015,by=list(data_all$lc_group), FUN='mean')
# Group.1         x
# 1                    Africa 0.3062350
# 2                      Asia 0.3395723
# 3                    Europe 0.3984158
# 4          Northern America 0.3902476
# 5                   Oceania 0.3455268
# 6                      SIDS 0.3751165
# 7 South and Central America 0.3054498
mean(data_all$max_popweighted_2015)
#0.3404622

sum_lcd_2016<-table(data_all$indicator_2016,data_all$lc_group)
sum_lcd_2016<-as.data.frame(sum_lcd_2016)
aggregate(data_all$max_popweighted_2016,by=list(data_all$lc_group), FUN='mean')
# Group.1         x
# 1                    Africa 0.2528003
# 2                      Asia 0.3116826
# 3                    Europe 0.3972981
# 4          Northern America 0.3784589
# 5                   Oceania 0.3457911
# 6                      SIDS 0.3551049
# 7 South and Central America 0.2784914
mean(data_all$max_popweighted_2016)
#0.3159397

sum_lcd_2020<-table(data_all$indicator_2020,data_all$lc_group)
sum_lcd_2020<-as.data.frame(sum_lcd_2020)
aggregate(data_all$max_popweighted_2020,by=list(data_all$lc_group), FUN='mean')
# Group.1         x
# 1                    Africa 0.2971345
# 2                      Asia 0.3434128
# 3                    Europe 0.4023793
# 4          Northern America 0.4055245
# 5                   Oceania 0.3322532
# 6                      SIDS 0.3830969
# 7 South and Central America 0.3025674
mean(data_all$max_popweighted_2020)
#0.3417322

sum_lcd_2021<-table(data_all$indicator_2021,data_all$lc_group)
sum_lcd_2021<-as.data.frame(sum_lcd_2021)
aggregate(data_all$max_popweighted_2021,by=list(data_all$lc_group), FUN='mean')
# Group.1         x
# 1                    Africa 0.2936277
# 2                      Asia 0.3406854
# 3                    Europe 0.4001127
# 4          Northern America 0.3698068
# 5                   Oceania 0.3542351
# 6                      SIDS 0.3776210
# 7 South and Central America 0.3002303
mean(data_all$max_popweighted_2021)
#0.3383197

sum_lcd_2022<-table(data_all$indicator_2022,data_all$lc_group)
sum_lcd_2022<-as.data.frame(sum_lcd_2022)
aggregate(data_all$max_popweighted_2022,by=list(data_all$lc_group), FUN='mean')
#Group.1         x
# 1                    Africa 0.2901439
# 2                      Asia 0.3397801
# 3                    Europe 0.3944174
# 4          Northern America 0.4044978
# 5                   Oceania 0.3412551
# 6                      SIDS 0.3800693
# 7 South and Central America 0.2991945
mean(data_all$max_popweighted_2022)
#0.3372718

sum_lcd_2023<-table(data_all$indicator_2023,data_all$lc_group)
sum_lcd_2023<-as.data.frame(sum_lcd_2023)
aggregate(data_all$max_popweighted_2023,by=list(data_all$lc_group), FUN='mean')
# Group.1         x
# 1                    Africa 0.3025560
# 2                      Asia 0.3420911
# 3                    Europe 0.4043673
# 4          Northern America 0.3749149
# 5                   Oceania 0.3499703
# 6                      SIDS 0.3834807
# 7 South and Central America 0.2980968
mean(data_all$max_popweighted_2023)
#0.340341

#2015
data_all$peak.NDVI_2015<-as.numeric(data_all$peak.NDVI_2015)
a<-tapply(data_all$peak.NDVI_2015,data_all$lc_group, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2015,data_all$lc_group, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2015,data_all$lc_group, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2015, data_all$lc_group, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2015,na.rm=TRUE)
e2<-mean(data_all$cumavg_2015,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2015,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2015,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table2<-rbind(a,b,c,d)
table2<-t(table2)
table2<-rbind(table2,e)
table2<-round(table2,2)
table2<-as.data.frame(table2)
names(table2) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table2$year<-rep('2015',8)

#2016
data_all$peak.NDVI_2016<-as.numeric(data_all$peak.NDVI_2016)
a<-tapply(data_all$peak.NDVI_2016,data_all$lc_group, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2016,data_all$lc_group, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2016,data_all$lc_group, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2016, data_all$lc_group, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2016,na.rm=TRUE)
e2<-mean(data_all$cumavg_2016,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2016,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2016,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table3<-rbind(a,b,c,d)
table3<-t(table3)
table3<-rbind(table3,e)
table3<-round(table3,2)
table3<-as.data.frame(table3)
names(table3) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table3$year<-rep('2016',8)


#2020
data_all$peak.NDVI_2020<-as.numeric(data_all$peak.NDVI_2020)
a<-tapply(data_all$peak.NDVI_2020,data_all$lc_group, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2020,data_all$lc_group, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2020,data_all$lc_group, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2020, data_all$lc_group, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2020,na.rm=TRUE)
e2<-mean(data_all$cumavg_2020,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2020,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2020,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table4<-rbind(a,b,c,d)
table4<-t(table4)
table4<-rbind(table4,e)
table4<-round(table4,2)
table4<-as.data.frame(table4)
names(table4) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table4$year<-rep('2020',8)

#2021
data_all$peak.NDVI_2021<-as.numeric(data_all$peak.NDVI_2021)
a<-tapply(data_all$peak.NDVI_2021,data_all$lc_group, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2021,data_all$lc_group, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2021,data_all$lc_group, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2021, data_all$lc_group, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2021,na.rm=TRUE)
e2<-mean(data_all$cumavg_2021,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2021,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2021,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table5<-rbind(a,b,c,d)
table5<-t(table5)
table5<-rbind(table5,e)
table5<-round(table5,2)
table5<-as.data.frame(table5)
names(table5) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table5$year<-rep('2021',8)

#2022
data_all$peak.NDVI_2022<-as.numeric(data_all$peak.NDVI_2022)
a<-tapply(data_all$peak.NDVI_2022,data_all$lc_group, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2022,data_all$lc_group, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2022,data_all$lc_group, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2022, data_all$lc_group, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2022,na.rm=TRUE)
e2<-mean(data_all$cumavg_2022,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2022,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2022,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table6<-rbind(a,b,c,d)
table6<-t(table6)
table6<-rbind(table6,e)
table6<-round(table6,2)
table6<-as.data.frame(table6)
names(table6) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table6$year<-rep('2022',8)


#2023
data_all$peak.NDVI_2023<-as.numeric(data_all$peak.NDVI_2023)
a<-tapply(data_all$peak.NDVI_2023,data_all$lc_group, mean, na.rm=TRUE)
b<-tapply(data_all$cumavg_2023,data_all$lc_group, mean, na.rm=TRUE)
c<-tapply(data_all$max_popweighted_2023,data_all$lc_group, mean, na.rm=TRUE)
d<-tapply(data_all$cum_popweighted_avg_2023, data_all$lc_group, mean, na.rm=TRUE)
data_all<-na.omit(data_all)

e1<-mean(data_all$peak.NDVI_2023,na.rm=TRUE)
e2<-mean(data_all$cumavg_2023,na.rm=TRUE)
e3<-mean(data_all$max_popweighted_2023,na.rm=TRUE)
e4<-mean(data_all$cum_popweighted_avg_2023,na.rm=TRUE)
e<-cbind(e1,e2,e3,e4)
row.names(e)<-"Global Mean"

table7<-rbind(a,b,c,d)
table7<-t(table7)
table7<-rbind(table7,e)
table7<-round(table7,2)
table7<-as.data.frame(table7)
names(table7) <- c("Peak NDVI", "Four-season NDVI", "Pop weighted Peak NDVI", "Pop weighted Four-season NDVI")
table7$year<-rep('2023',8)


table1<-rbind(table2,table3,table4,table5,table6,table7)

write.csv(table1, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/table1_lcd.csv")