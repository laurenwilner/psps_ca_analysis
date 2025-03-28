#-------------------------------------------------
# PSPS: Link PSPS data (numerator, denominator, wf data)
# zcta level
# November 2024
#-------------------------------------------------
# setup
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(sf, tigris, MetBrewer, lubridate, arrow, raster, tidyverse, scales, knitr, kableExtra, patchwork)

CRS = 'EPSG:3310' # this is california albers

raw_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/raw/psps_circuit_data/")
raw_raster_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/raw/")
clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
plot_dir <- ("~/Desktop/Desktop/epidemiology_PhD/02_projects/psps/plots/")
psps_file_name <- '2023.07.clean_psps_data.csv'
repo <- "~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/"

washout <- FALSE
wf_pm_threshold <- 15 # threshold for wildfire pm2.5 but should look at distribution before finalizing

#-------------------------------------------------
# helper functions
# collapse events that occur within one week of each other
collapse_events_rle <- function(df) {
  df <- df %>%
    arrange(zcta, outage_start) %>%
    mutate(event_group = 0)
  current_group <- 1
  unique_zctas <- unique(df$zcta)
  for (z in unique_zctas) {
    zcta_events <- df %>% filter(zcta == z)
    for (i in seq_len(nrow(zcta_events))) {
      if (zcta_events$event_group[i] == 0) {
        zcta_events$event_group[i] <- current_group
        j <- i + 1
        while (j <= nrow(zcta_events) && zcta_events$outage_start[j] <= zcta_events$outage_start[i] + days(7)) {
          zcta_events$event_group[j] <- current_group
          j <- j + 1
        }
        current_group <- current_group + 1
      }
    }
    df <- df %>%
      filter(zcta != z) %>%
      bind_rows(zcta_events)
  }
  df <- df %>%
    group_by(zcta, event_group) %>%
    summarize(
      psps_event_id = first(psps_event_id),
      outage_start = min(outage_start),
      outage_end = max(outage_end),
      total_customers_impacted = sum(total_customers_impacted), # im not sure about recalculating this here
      duration = difftime(outage_end, outage_start, unit = "hours"), # i think we want to recalc duration here
      pop = first(pop),
      pct_cust_out = mean(pct_cust_out)
    ) %>%
    ungroup() %>%
    select(-event_group) 
  
  return(df)
}

# exclude events more than one week but less than four weeks apart
exclude_events_rle <- function(df) {
  df <- df %>%
    arrange(zcta, outage_start) %>%
    group_by(zcta) %>%
    mutate(
      event_diff = lead(outage_start) - outage_start,
      event_exclude = if_else(event_diff > days(7) & event_diff < days(28), TRUE, FALSE) # using 28 days as a 'month'
      # excluding the event prior to the event that is in exclusion time period 
    ) %>%
    filter(!event_exclude | is.na(event_exclude)) %>%
    ungroup() %>%
    select(-event_diff, -event_exclude)
  return(df)
}

#-------------------------------------------------
# read in data

# these files are the outputs of python script called XXX 
denom <- read_parquet(paste0(clean_dir, "ca_gridded_zcta_pop.parquet"))
num <- read_parquet(paste0(clean_dir, "ca_zcta_psps_customers_impacted.parquet"))

# xwalk 
zip_to_zcta_xwalk <- read.csv(paste0(repo, "zip_zcta_xref.csv")) %>% 
    mutate(zip_code = str_pad(zip_code, 5, side = "left", pad = 0)) %>%
    select(zip_code, zcta)

# wf data
wf_ca <- read_csv(paste0(raw_dir, "../CAzip_wf_pm25_2006to2020.csv")) %>% 
    mutate(zip_code = str_pad(zip_code, 5, side = "left", pad = 0)) %>%
    left_join(zip_to_zcta_xwalk, by = "zip_code") %>%
    mutate(zcta = str_pad(zcta, 5, side = "left", pad = 0)) %>% 
    select(zcta, date, wf_pm25)

#-------------------------------------------------
# construct clean dataset
psps_clean <- num %>% 
            left_join(denom, by = "zcta") %>% 
            mutate(
                outage_start = as.POSIXct(outage_start,
                    format = "%m/%d/%Y %H:%M",
                    tz = "America/Los_Angeles"),
                outage_end = as.POSIXct(outage_end,
                    format = "%m/%d/%Y %H:%M",
                    tz = "America/Los_Angeles"),
                pct_cust_out = total_customers_impacted/pop,
                duration = as.numeric(duration, units = "hours"),
                pct_cust_out = ifelse(pct_cust_out > 1, 1, pct_cust_out)) # if more than 100% of the population is out, set to 100%

# step 3: include/exclude based on washout period: 
    # anything within a week prior to an event gets collapsed to one event. 
    # anything >1 and <4 weeks prior gets excluded.

if(washout == TRUE){
  psps_washout <- collapse_events_rle(psps_clean)
  psps_analysis <- exclude_events_rle(psps_washout)
} else{
  psps_analysis <- psps_clean
}

#-------------------------------------------------
# step 4: merge on wf data
# merging on the wf pm from the start date of the fire

wf_ca_collapse <- wf_ca %>% 
    group_by(zcta, date) %>% 
    summarise(wf_pm25 = mean(wf_pm25, na.rm = TRUE)) %>% 
    ungroup()
psps_analysis <- psps_analysis %>% 
    mutate(date = date(outage_start)) %>%
    left_join(wf_ca_collapse, by = c("zcta", "date")) %>% 
    mutate(wf_exposed = ifelse(wf_pm25 >= wf_pm_threshold, 1, 0)) %>% 
    select(-c(date, wf_pm25))

#-------------------------------------------------
# step 5: expand out to hourly dataset         
# expand back to hourly dataset
psps_analysis_hourly <- psps_analysis %>%
  rowwise() %>%
  mutate(hour = list(seq(from = floor_date(outage_start, "hour"),
                         to = ceiling_date(outage_end, "hour") - hours(1),
                         by = "hour"))) %>%
  unnest(hour) %>%
  ungroup() %>%
  mutate(hours_since_start = as.numeric(difftime(hour, outage_start, units = "hours"))) %>% 
  mutate(hours_since_start = ifelse(hours_since_start < 0, 0, hours_since_start))

if(washout == TRUE){
  write.csv(psps_analysis_hourly, paste0(clean_dir, "ca_zcta_hourly_psps_washout_2013-2022.csv"))
  write.csv(psps_analysis, paste0(clean_dir, "ca_zcta_daily_psps_washout_2013-2022.csv"))
} else{
  write.csv(psps_analysis_hourly, paste0(clean_dir, "ca_zcta_hourly_psps_no_washout_2013-2022.csv"))
  write.csv(psps_analysis, paste0(clean_dir, "ca_zcta_daily_psps_no_washout_2013-2022.csv"))
}