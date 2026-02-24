#########################################################################
# Numbers referenced in appendix
#########################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)

####import####
#import all the yearly data files
#set working directory
setwd('~/Documents/data/Lancet 2025/')

un_countries = read.csv("groupings/UNSD — Methodology.csv", sep = ";")

country_list<-read_excel("groupings/2026 Guidance_Country Names and Groupings.xlsx", range = "A2:E222")
lcd2025<-read.csv("output/lcd2025.csv")

#countries not in the analysis
country_list$`Country Name to use`[!country_list$`ISO3` %in% lcd2025$ISO3]

un_countries$`Country.or.Area`[!un_countries$`ISO.alpha3.Code` %in% lcd2025$ISO3]


