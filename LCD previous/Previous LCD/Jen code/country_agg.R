library(tidyr)
library(dplyr)

cities<-read.csv("C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/greeness_cities_2021.csv")

cities$peak.NDVI_2021<-as.numeric(cities$peak.NDVI_2021)

cities<-cities %>% 
  mutate(green_level = recode(indicator_2021, 
                                 "Exceptionally Low" = "1",
                                 "Very Low" = "2",
                                 "Low" = "3",
                                 "Moderate" = "4",
                                 "High" = "5",
                                 "Very High" = "6",
                                 "Exceptionally High" = "7"))

cities$green_level<-as.numeric(cities$green_level)

country<-aggregate(cities, by=list(cities$Country), FUN = "mean")

country<-country[,-c(2:9,11,13:17)]

country<-country %>%
  mutate(greenness=round(green_level,0))

country<-country[,-4]

names(country)<-c('Country','Peak_NDVI','PopWeight_Peak_NDVI','Greenness_Level')

country<-country %>% 
  mutate(Greenness_Level = recode(Greenness_Level, 
                              "1"="Exceptionally Low",
                              "2"="Very Low",
                              "3"="Low",
                              "4"="Moderate",
                              "5"="High",
                              "6"="Very High",
                              "7"="Exceptionally High"))

write.csv(country, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/country_peak_greenness_2022.csv")
