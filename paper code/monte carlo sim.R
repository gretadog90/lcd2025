################################################################################
# Run Monte Carlo Simulation for estimating uncertainty
################################################################################
#### Set up ####
#clear objects
rm(list = ls()) 

#libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringi)
library(data.table)

#set working directory
setwd('~/Documents/data/Lancet 2025/')

#set seed for reproducible results
set.seed(122690)

#read in info for MC analysis
mc<-read.csv("outputHIA/mc.csv")
mc<-mc[mc$city !="Kapoeta",]

################################################################################
# First run 1000 simulations of a draw from normal distribution for hazard ratio
################################################################################
hr<-0.96 #mean from rojas-rueda paper

#calculate sd: using ub and lb of HR from rojas-rueda paper
# and by 2* critical value for 95%CI (1.96)
sd_hr<- (log(0.97)-log(0.94))/(2*1.96)

# list to store simulated HRs
hazard_ratios <- numeric(10000)

# Perform Monte Carlo simulations
for (i in 1:10000) {
  # Generate a sample from a normal distribution
  draw <- rnorm(1, mean = hr, sd = sd_hr)
  
  # store that draw
  hazard_ratios[i] <- draw
}
################################################################################
# Now loop through individual cities to simulate baseline mortality draws from
# normal distribution, pull in HR draw, and run HIA
################################################################################
#split data set by city ID so that each city now has a seprate one row data set
lcd_cities<-split(mc, mc$city)

#get list of all city IDs to loop through
cities<-mc$city

#loop through cities
for(city in cities)  {
  #temporarily store city info to dataframe
  data <- lcd_cities[[city]] 
  
  # Parameters for GBD baseline mortality simulation
  mean<-data$val.2020
  sd<-(data$upper.2020-data$lower.2020)/(2*1.96)
  
  #expand data set so that there are 1,000 rows
  expanded_df <- expand(data, ID_HDC_G0, diff, Population_2020_100m, val.2020, sequence = 1:10000) 
  
  # list to store simulated mortality rates
  baseline_mortality_rates <- numeric(10000)
  
  # Perform Monte Carlo simulations
  for (i in 1:10000) {
    # Generate a sample from a normal distribution
    draw<- rnorm(1, mean = mean, sd = sd)
    
    # Calculate the mean of the sample
    baseline_mortality_rates[i] <- draw
  }
  
  #merge simulated HRs and simulated BMRs
  new_df<-do.call(rbind, Map(data.frame, sequence = 1:10000, hrs=hazard_ratios, bmrs=baseline_mortality_rates))
  
  #merge to city info
  expanded_df<-merge(expanded_df, new_df, by='sequence')

  #run the HIA
  expanded_df$paf<-1-(1/expanded_df$hrs^expanded_df$diff) 
  expanded_df$delta_mortality=expanded_df$bmrs*expanded_df$Population_2020_100m*expanded_df$paf

  #get the mean, lb, and ub from the 1000 simulations
  data$lower_ci <- quantile(expanded_df$delta_mortality, 0.025)
  data$upper_ci <- quantile(expanded_df$delta_mortality, 0.975)
  data$mean<- mean(expanded_df$delta_mortality)
  
  #save city datasets
  saveRDS(data, file=paste0("tmp/",city,".rds"))
  
  #clean excess memory
  gc()
}
  