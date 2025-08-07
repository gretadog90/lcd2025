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

#read in summary data to get regional/climate info
summary_lcd<-read.csv("output/lcd2025.csv")
summary_lcd<-summary_lcd[,c("ID_HDC_G0", "city", "country", "sub.region", "clim_region")]
summary_lcd<-summary_lcd[summary_lcd$city !="Kapoeta",]

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
  saveRDS(expanded_df, file=paste0("tmp/",city,"_allruns.rds"))
  saveRDS(data, file=paste0("tmp/",city,"_mean.rds"))
  
  #clean excess memory
  gc()
}

################################################################################
# append all the city runs together since results aren't reported at city level
# but by region/climate zone/overall
################################################################################
all<-data.frame()
city_summary<-data.frame()

#loop through cities
for(city in cities) {
  #open data with census and exposure measures
  data<-readRDS(paste0("tmp/", city,"_allruns.rds"))
  summary<-readRDS(paste0("tmp/", city,"_mean.rds"))
  
  #save all the data together for graphing/summary stats
  all<-rbind(all, data)
  city_summary<-rbind(city_summary, summary)
  
  #clean excess memory
  gc()
}

#save city datasets
saveRDS(all, file=paste0("tmp/mc_all.rds"))
saveRDS(city_summary, file=paste0("tmp/mc_city_summary.rds"))

################################################################################
#data mgmt to get overall and regional means 
################################################################################
all <-readRDS("tmp/mc_all.rds")

#merge to data file to get sub.region and clim_region
all<-merge(all, summary_lcd, by="ID_HDC_G0", all.x=TRUE)

#get per 100,000 pop mortality
all$delta_mortality_100<-(all$delta_mortality/all$Population_2020_100m)*100000

# get overall CIs
simulation_means<- all %>% 
  group_by(sequence)  %>% 
  dplyr::summarize(
    mean=mean(delta_mortality),
    mean_std=mean(delta_mortality_100),
    total_deaths=sum(delta_mortality)
  )

lower_ci_total=quantile(simulation_means$total_deaths, 0.025)
upper_ci_total=quantile(simulation_means$total_deaths, 0.975)
mean_total=mean(simulation_means$total_deaths)
lower_ci_std=quantile(simulation_means$mean_std, 0.025)
upper_ci_std=quantile(simulation_means$mean_std, 0.975)

# get regional CIs
simulation_means_region<- all %>% 
  group_by(sub.region, sequence)  %>% 
  dplyr::summarize(
    total_deaths=sum(delta_mortality),
    total_pop=sum(Population_2020_100m)
  )

#get per 100,000 pop mortality
simulation_means_region$delta_mortality_100<-(simulation_means_region$total_deaths/simulation_means_region$total_pop)*100000

#now summarize to region
simulation_means_region_std<- simulation_means_region %>% 
  group_by(sub.region)  %>% 
  dplyr::summarize(
    mean=mean(total_deaths),
    lower_ci=quantile(total_deaths, 0.025),
    upper_ci=quantile(total_deaths, 0.975),
    mean_std=mean(delta_mortality_100),
    lower_ci_std=quantile(delta_mortality_100, 0.025),
    upper_ci_std=quantile(delta_mortality_100, 0.975)
  )
    
# get climate zone CIs
simulation_means_clim<- all %>% 
  group_by(clim_region, sequence)  %>% 
  dplyr::summarize(
    total_deaths=sum(delta_mortality),
    total_pop=sum(Population_2020_100m)
  )

#get per 100,000 pop mortality
simulation_means_clim$delta_mortality_100<-(simulation_means_clim$total_deaths/simulation_means_clim$total_pop)*100000

#now summarize to region
simulation_means_clim_std<- simulation_means_clim %>% 
  group_by(clim_region)  %>% 
  dplyr::summarize(
    mean=mean(total_deaths),
    lower_ci=quantile(total_deaths, 0.025),
    upper_ci=quantile(total_deaths, 0.975),
    mean_std=mean(delta_mortality_100),
    lower_ci_std=quantile(delta_mortality_100, 0.025),
    upper_ci_std=quantile(delta_mortality_100, 0.975)
  )
