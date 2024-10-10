#########################################################################
# Figure 4: HIA relative contributions
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

#read in HIA results
hia<-read.csv("outputHIA/hia_100m.csv")

#create % of HIA contribution vars
hia$pct_