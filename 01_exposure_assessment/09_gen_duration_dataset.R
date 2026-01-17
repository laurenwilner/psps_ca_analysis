#-------------------------------------------------
# PSPS: Create duration dataset
# author: Lauren Blair Wilner
# December 2025
#-------------------------------------------------

#-------------------------------------------------
# setup
library(pacman)
p_load(tidyverse, lubridate, slider)
clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")

df <- read_csv(paste0(clean_dir, 'ca_ZIP_daily_psps_no_washout_wf_classified_2013-2022.csv')) %>%
  select(zip_code, outage_start, outage_end, psps_event_id)

# Function to expand one row into hourly records
expand_to_hourly <- function(zip, start, end) {
  # Round start down to the hour, end up to the hour
  start_hour <- floor_date(start, "hour")
  end_hour <- ceiling_date(end, "hour")
  
  # Create sequence of hours
  hours <- seq(start_hour, end_hour - hours(1), by = "hour")
  
  tibble(zip_code = zip, hour = hours)
}

# Expand each event to hourly
hourly <- df %>%
  rowwise() %>%
  reframe(expand_to_hourly(zip_code, outage_start, outage_end))

# Deduplicate - each zip-hour combo only counts once
hourly_deduped <- hourly %>%
  distinct(zip_code, hour)

# Collapse to daily counts
duration_df <- hourly_deduped %>%
  mutate(date = as_date(hour)) %>%
  group_by(zip_code, date) %>%
  summarize(outage_hours = n(), .groups = "drop")

# make each day actually be a 7-day lag, so day of interest + 6 days prior averaged
duration_df <- duration_df %>%
  group_by(zip_code) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  mutate(outage_hours = replace_na(outage_hours, 0)) %>%
  arrange(zip_code, date) %>%
  mutate(
    outage_hours_lag7 = slide_dbl(
      outage_hours,
      sum,
      .before = 6,
      .complete = FALSE
    )
  ) %>%
  ungroup() %>% 
  filter(outage_hours_lag7 > 0)


# look at just nov for zip 90264 to see if its right 
# nov_df <- duration_df %>%
#   filter(zip_code == 90264, date >= as_date("2019-11-01"), date <= as_date("2019-11-30"))
# print(nov_df)

write_csv(duration_df, paste0(clean_dir, 'ca_ZIP_daily_psps_no_washout_wf_classified_2013-2022_duration_exp.csv'))