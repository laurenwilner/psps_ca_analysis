#-------------------------------------------------
# PSPS data cleaning
# January 2024

#-------------------------------------------------
# setup
library(tidyverse)
library(sf)
library(tigris)
setwd("~/Desktop/Desktop/epidemiology_PhD/data/raw/2023.07.psps_data/")

# NOTE: subsetting to only "big" outages: >= 8 hours OR >= 50% of pop
pct_pop_threshold <- .5
hr_threshold <- 8

#-------------------------------------------------
# Download all counties and tracts in CA
counties <- counties(state = "CA", year = 2010)
tracts <- tracts(state = "CA", year = 2010)

#-------------------------------------------------
# Import & clean PSPS data from Bethany Kwoka at PSE
psps_wide <-
  read_csv("2023.07.psps_outages_by_census_tract.csv") %>%
  rename(Latest_Outage_Restoration = Lastest_Outage_Restoration) %>%
  mutate(GEOID10 = str_pad(GEOID, 11, pad = "0")) %>%
  select(c(GEOID10, Sub_Event_ID, PSPS_Event_ID, Earliest_Outage_Start,
          Latest_Outage_Restoration, Total_Population_CES,
          Customer_Weighted_Duration_Hrs, circuit_intersect_pct,
          Customers_Impacted, Circuit_Name_ICA)) %>%
  rename_with(tolower) %>%
  filter(total_population_ces!=0) %>%
  # note: there are 19 rows with 0 pop
  filter((customers_impacted / total_population_ces) >= pct_pop_threshold |
           customer_weighted_duration_hrs >= hr_threshold)

psps_wide <- psps_wide %>%
  mutate(start_time = as.POSIXct(earliest_outage_start,
        format="%m/%d/%Y %H:%M"),
         end_time = as.POSIXct(latest_outage_restoration,
        format="%m/%d/%Y %H:%M"),
        event_hr = as.numeric(
          difftime(end_time, start_time, units = "hours"))) %>%
  select(-c("earliest_outage_start", "latest_outage_restoration"))

#-------------------------------------------------
# Reshape long (1 row per outage day)
psps_long <- psps_wide %>%
  rowwise() %>%
  mutate(day = list(seq(date(start_time), date(end_time), 1))) %>%
  ungroup() %>%
  unnest(day) %>%
  distinct

psps_long <- psps_long %>%
  mutate(start = ifelse(
          day == date(start_time),
          start_time,
          as.POSIXct(paste(day, "00:00:00"), format="%Y-%m-%d %H:%M:%S")),
         end = ifelse(
          day == date(end_time),
          end_time,
          as.POSIXct(paste(day+1, "00:00:00"), format="%Y-%m-%d %H:%M:%S"))) %>%
  mutate(across(c(start, end), ~as.POSIXct(.x, origin = "1970-01-01"))) %>%
  mutate(daily_out_hr = as.numeric(difftime(end, start, units = "hours"))) %>%
  select(c('geoid10', 'start', 'end', 'psps_event_id', 
           'sub_event_id', 'circuit_name_ica',
           'customers_impacted', 'daily_out_hr', 'day',
           'total_population_ces'))
# NOTE
  # 3 instances of 25 hour days on Nov 1 2020 -- time changed on this day
  # TODO: no duration vars left, need to figure duration out with the team

#-------------------------------------------------
# Collapse to tract level (from subevents w/in tracts)
daily_psps_tract <- psps_long %>%
  group_by(geoid10, total_population_ces, day) %>%
  # is there a reason we have total pop here? 
  summarize(event_id = toString(unique(psps_event_id)),
            circuit = toString(unique(circuit_name_ica)),
            earliest_out = min(start),
            latest_out = max(end),
            tot_custout = sum(customers_impacted), # is this unique
            ) %>%
  mutate(pct_pop_out = (tot_custout / total_population_ces) * 100) %>%
  ungroup()

# Collapse to county level
daily_psps_county <- psps_long %>% 
  mutate(countyfp10 = substr(geoid10, 3, 5)) %>%
  group_by(countyfp10, day) %>%
  summarize(event_id = toString(unique(psps_event_id)),
            circuit = toString(unique(circuit_name_ica)),
            earliest_out = min(start),
            latest_out = max(end),
            tot_custout = sum(customers_impacted),
            total_population_ces = sum(total_population_ces)) %>%
  mutate(pct_pop_out = (tot_custout / total_population_ces) * 100) %>%
  ungroup()

#-------------------------------------------------
write.csv(daily_psps_tract,
  "~/Desktop/Desktop/epidemiology_PhD/data/clean/ca_psps_daily_by_tract.csv",
  row.names = FALSE)
write.csv(daily_psps_county,
  "~/Desktop/Desktop/epidemiology_PhD/data/clean/ca_psps_daily_by_county.csv",
  row.names = FALSE)


# TODO:
  # include business counts in denominator
  # check: are denominators double counting circuit pops?
  # ID scenarios where neighboring tracts have a consistently
  # high # of customers out for 8+ hrs
  # find 3-4 examples - map these and find neighbors
