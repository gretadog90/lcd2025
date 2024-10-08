rm(list = ls())

library(tidyr)
library(dplyr)

fall_2023<-read.csv("C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/GEE Data Files/2023_Fall_AllCities.csv")
colnames(fall_2023)
fall_2023<-fall_2023[,c(329,32,334,341)]
fall_2023<-fall_2023[order(fall_2023$Urban_area),]


winter_2023<-read.csv("C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/GEE Data Files/2023_Winter_AllCities.csv")
colnames(winter_2023)
winter_2023<-winter_2023[,c(329,32,334,341)]
winter_2023<-winter_2023[order(winter_2023$Urban_area),]


spring_2023<-read.csv("C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/GEE Data Files/2023_Spring_AllCities.csv")
colnames(spring_2023)
spring_2023<-spring_2023[,c(329,32,334,341)]
spring_2023<-spring_2023[order(spring_2023$Urban_area),]


summer_2023<-read.csv("C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/GEE Data Files/2023_Summer_AllCities.csv")
colnames(summer_2023)
summer_2023<-summer_2023[,c(329,32,334,341)]
summer_2023<-summer_2023[order(summer_2023$Urban_area),]


all_seasons_2023<-rbind(fall_2023,winter_2023,spring_2023,summer_2023)
all_seasons_2023<-aggregate(all_seasons_2023, by=list(all_seasons_2023$Urban_area,
                                                      all_seasons_2023$Country,
                                                      all_seasons_2023$XC_ISO_LST),FUN=mean, na.rm=TRUE)
names(all_seasons_2023)[7]<-"annual_avg_2023"
names(all_seasons_2023)[1:3]<-c('City','Country','XC_ISO_LST')
all_seasons_2023<-all_seasons_2023[,-c(4:6)]
all_seasons_2023<-all_seasons_2023[order(all_seasons_2023$Country,all_seasons_2023$City),]


all_season_2023_max<-rbind(fall_2023,winter_2023,spring_2023,summer_2023)
all_season_2023_max<-aggregate(all_season_2023_max, by=list(all_season_2023_max$Urban_area,
                                                            all_season_2023_max$Country,
                                                            all_season_2023_max$XC_ISO_LST),FUN=max, na.rm=TRUE)
all_season_2023_max[289,7]<-"NA"

names(all_season_2023_max)[7]<-"max_NDVI_2023"
names(all_season_2023_max)[1:3]<-c('City','Country','XC_ISO_LST')
all_season_2023_max<-all_season_2023_max[,-c(4:6)]
all_season_2023_max<-all_season_2023_max[order(all_season_2023_max$Country,all_season_2023_max$City),]
mean(as.numeric(all_season_2023_max$max_NDVI_2023),na.rm=TRUE)
#0.3529246

cumavg_max_2023<-cbind(all_season_2023_max,all_seasons_2023)
cumavg_max_2023<-cumavg_max_2023[,-c(5:7)]
names(cumavg_max_2023)
write.csv(cumavg_max_2023,"C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/2023_cumavg_max.csv")


###########################################

fall_2023sum<-read.csv("C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/GEE Data Files/2023_sum1_fall_AllCities.csv")
colnames(fall_2023sum)
fall_2023sum<-fall_2023sum[,c(329,32,334,342)]
fall_2023sum<-fall_2023sum[order(fall_2023sum$Country,fall_2023sum$Urban_area),]

winter_2023sum<-read.csv("C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/GEE Data Files/2023_sum1_winter_AllCities.csv")
colnames(winter_2023sum)
winter_2023sum<-winter_2023sum[,c(329,32,334,342)]
winter_2023sum<-winter_2023sum[order(winter_2023sum$Country,winter_2023sum$Urban_area),]

spring_2023sum<-read.csv("C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/GEE Data Files/2023_sum1_spring_AllCities.csv")
colnames(spring_2023sum)
spring_2023sum<-spring_2023sum[,c(329,32,334,342)]
spring_2023sum<-spring_2023sum[order(spring_2023sum$Country,spring_2023sum$Urban_area),]

summer_2023sum<-read.csv("C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/GEE Data Files/2023_sum1_summer_AllCities.csv")
colnames(summer_2023sum)
summer_2023sum<-summer_2023sum[,c(329,32,334,342)]
summer_2023sum<-summer_2023sum[order(summer_2023sum$Country,summer_2023sum$Urban_area),]


all_seasons_2023sum<-rbind(fall_2023sum,winter_2023sum,spring_2023sum,summer_2023sum)
all_seasons_2023sum<-aggregate(all_seasons_2023sum, by=list(all_seasons_2023sum$Urban_area,all_seasons_2023sum$Country,
                                                            all_seasons_2023sum$XC_ISO_LST),FUN=mean)

names(all_seasons_2023sum)[7]<-"cumavg_Sum1_2023"
names(all_seasons_2023sum)[1:3]<-c('City','Country','XC_ISO_LST')
all_seasons_2023sum<-all_seasons_2023sum[,-c(4:6)]
all_seasons_2023sum<-all_seasons_2023sum[order(all_seasons_2023sum$Country,all_seasons_2023sum$City),]
write.csv(all_seasons_2023sum,"C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/all_seasons_2023sum.csv")


all_season_2023_max<-rbind(fall_2023sum,winter_2023sum,spring_2023sum,summer_2023sum)
all_season_2023_max<-aggregate(all_season_2023_max, by=list(all_season_2023_max$Urban_area,all_season_2023_max$Country,
                                                            all_season_2023_max$XC_ISO_LST),FUN=max)

names(all_season_2023_max)[7]<-"max_Sum1_2023"
names(all_season_2023_max)[1:3]<-c('City','Country','XC_ISO_LST')
all_season_2023_max<-all_season_2023_max[,-c(4:6)]
all_season_2023_max<-all_season_2023_max[order(all_season_2023_max$Country,all_season_2023_max$City),]
write.csv(all_season_2023_max,"C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/all_seasons_2023max.csv")


sum2_2023<-read.csv("C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/GEE Data Files/2020_sum2_popsize_AllCities.csv")
colnames(sum2_2023)
sum2_2023<-sum2_2023[,c(329,32,334,342)]
names(sum2_2023)[4]<-"sum2"
names(sum2_2023)[1]<-"City"
sum2_2023<-sum2_2023[order(sum2_2023$Country,sum2_2023$City),]

pop_cumav_2023<-cbind(all_seasons_2023sum,all_season_2023_max)
pop_cumav_2023<-pop_cumav_2023[,-c(5:7)]
 
pop_cumav_2023<-cbind(pop_cumav_2023,sum2_2023)
names(pop_cumav_2023)
pop_cumav_2023<-pop_cumav_2023[,-c(6:8)]
#pop_cumav_2023<-pop_cumav_2023[,-c(7:352)]


pop_cumav_2023$cum_popweighted_avg<-pop_cumav_2023$cumavg_Sum1_2023/pop_cumav_2023$sum2
pop_cumav_2023$max_popweighted<-pop_cumav_2023$max_Sum1_2023/pop_cumav_2023$sum2

write.csv(pop_cumav_2023,"C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/Lancet 2024/2023_popweighted_cumavg_max.csv")
