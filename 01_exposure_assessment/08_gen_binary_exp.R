#-------------------------------------------------
# PSPS: Create binary exposure variables for paper
# author: Lauren Blair Wilner
# May 2025

#-------------------------------------------------

#-------------------------------------------------
# setup
library(tidyverse)

psps_levels <- read_csv("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/ca_ZIP_daily_psps_no_washout_classified_2013-2022.csv")

#--------------------------------------------------
# format date-time
psps_levels$outage_start <- as.POSIXct(psps_levels$outage_start, format="%Y-%m-%d %H:%M:%S")

#--------------------------------------------------
# generate daily exp
# transform into daily records from date-time
daily_psps <- function(start_datetime, end_datetime, event_id, zip_code, severity_customers, severity_hybrid) {
  # Extract dates
  start_date <- as.Date(start_datetime)
  end_date <- as.Date(end_datetime)
  data.table(
    date = seq.Date(start_date, end_date, by = "day"),
    psps_event_id = event_id,
    zip_code = zip_code,
    severity_customers = severity_customers,
    severity_hybrid = severity_hybrid
  )
}

# apply the function to each event
daily_exposure <- rbindlist(
  lapply(1:nrow(psps_levels), function(i) {
    daily_psps(
      psps_levels$outage_start[i],
      psps_levels$outage_end[i],
      psps_levels$psps_event_id[i],
      psps_levels$zip_code[i],
      psps_levels$severity_customers[i],
      psps_levels$severity_hybrid[i]
    )
  }))

# days in 2018-2019
dates <- data.table(date = seq.Date(as.Date("2013-01-01"), as.Date("2019-12-31"), by = "day"))

psps_daily <- left_join(dates, daily_exposure, by = "date") %>% 
  dplyr::select(c("date", "zip_code", "severity_customers", "severity_hybrid")) %>% 
  mutate(psps_abs = ifelse(is.na(severity_customers), 0, 1)) %>%
  mutate(psps_hybrid = ifelse(is.na(severity_hybrid), 0, 1)) %>% 
  filter(!is.na(zip_code)) %>%
  dplyr::select(-c("severity_customers", "severity_hybrid"))

#--------------------------------------------------
# write out the daily PSPS binary exposure data
write.csv(psps_daily, "~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/data/daily_psps_binary.csv", row.names = FALSE)
