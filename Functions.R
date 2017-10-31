####################################
# SENSOR SUMMARY FUNCTIONS
####################################

####################################
# Libraries

library(tidyverse)
library(lubridate)

####################################
# Function_1 - trim and add needed time variables

trim_n_time <- function(file_loc){
  
  # read in data
  sens_dat <- read.csv(file_loc) 
  
  # Trim files Midnight to Midnight
  sens_dat$date_time <- ymd_hms(sens_dat$date_time)
  m2m_st <- floor_date(sens_dat[1, "date_time"], unit = "days") + days(1)
  last_ep <- sens_dat[nrow(sens_dat), "date_time"]
  m2m_end <- (floor_date(last_ep, unit = "day")) - minutes(1)
  
  MtoM_ends <- interval(m2m_st, m2m_end)
  
  sens_MtoM <- sens_dat[which(sens_dat$date_time %within% MtoM_ends), ]
  
  ### extracts minute and hour of the day into separate variables
  sens_MtoM$min_of_day <- (hour(sens_MtoM$date_time) * 60) + (minute(sens_MtoM$date_time) + 1) # minute of day 
  sens_MtoM$hr_of_day <- hour(sens_MtoM$date_time) # hour of day
  
  ### makes these previous factor variables
  sens_MtoM$min_of_day <- factor(sens_MtoM$min_of_day)
  sens_MtoM$hr_of_day <- factor(sens_MtoM$hr_of_day)
  
  ### tells us how many 24hour periods are in the file
  num_24hr_days <- sum(sens_MtoM$min_of_day == 1440) # this is a pretty kludgey way to do this. Find something better.
  
  ### Numbers the 24hour periods
  sens_MtoM$day_num <- rep(1:num_24hr_days, each = 1440)
  sens_MtoM$day_date <- rep(seq(m2m_st, length = max(sens_MtoM$day_num), by = "day"), each = 1440)
  return(sens_MtoM)
}

