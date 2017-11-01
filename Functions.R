####################################
# SENSOR SUMMARY FUNCTIONS
####################################

####################################
# Libraries

library(tidyverse)
library(lubridate)

####################################
# Trim n Time function

# Argument "File loc" = location of the file you're reading in, relative to the working directory.

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



####################################
# Average reading per minute

# Argument "dat" = data you want to evaluate
# Argument "streams" = columns you want included in the summary

avg_24h_minute <- function(dat, streams){
  
avg_by_min <- dat %>% 
  group_by(min_of_day) %>%
  summarise_at(streams, mean, na.rm = T) %>%
  mutate_if(is.numeric, funs(round(.,2)))

avg_by_min$hr_of_day <- as.factor(rep(1:24, each = 60))

return(avg_by_min)
}



####################################
# Average reading per hour


# Argument "dat" = data you want to evaluate
# Argument "streams" = columns you want included in the summary

avg_24h_hour <- function(streams){

avg_lux_by_hr <- avg_24h_minute(streams) %>%
  group_by(hr_of_day) %>%
  summarise_at(streams, mean, na.rm = T) %>%
  mutate_if(is.numeric, funs(round(.,2)))

}




####################################
# Average time over threshold - NOT WORKING YET (move across from original script)

# Argument dat = data to evaluate
# Argument var_eval = variable to evaluate
# Argument thresholds = vector of thresholds to evaluate (add later)

# Use either mutate_at or summarise_at





####################################
# DRAFT ONLY
# Read in files Files should be named by their id only

load_all_csv <- function(loc) {
  
  files <- list.files(path = loc, pattern = "*.csv")
  
  for (i in 1:length(files)) {
  assign(gsub("*.csv$", "", files[i]), read.csv(paste(loc,files[i], sep = "")), inherits = T)}

  }
