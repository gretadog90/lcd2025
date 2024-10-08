#########################################################################
# Figure 1: Pop-weighted peak NDVI 2015, 2020, 2023
#########################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(dplyr)

####import####
#set working directory
setwd('~/Documents/data/Lancet 2025/')
output='graphs/'
