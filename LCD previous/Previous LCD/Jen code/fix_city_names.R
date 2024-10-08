library(stringr)

cities<-read.csv("C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/GHS Cities/GHS_2020_1042_edit.csv")

cities$city<-paste0(cities$UC_NM_LST,";")

cities$city1<-sub('(?<=\\;).*$','',cities$city,perl=TRUE)


cities$city1<-str_replace_all(cities$city1,"[;]",'')

write.csv(cities, "C:/Users/stowellj/OneDrive - Boston University/Lancet countdown/GHS Cities/GHS_2020_1042_CitiesTest.csv")
