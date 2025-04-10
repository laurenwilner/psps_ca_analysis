#-------------------------------------------------
# PSPS: Link PSPS to WF data (ZCTA)
# author: Lauren Blair Wilner
# updated: April 9, 2025

#-------------------------------------------------
# setup
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(sf, tigris, MetBrewer, lubridate, arrow, raster, tidyverse, scales, knitr, kableExtra, patchwork)

repo <- "~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/"
intermediate_dir <- paste0(repo, "data/intermediate/")
raw_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/raw/psps_circuit_data/")

wf_pm_threshold <- 15 # threshold for wildfire pm2.5 

#-------------------------------------------------
# read in data

# these files are the output of script 03.
if(washout == TRUE){
psps_temp <- read_csv(paste0(intermediate_dir, "ca_zcta_daily_psps_washout_2013-2022.csv"))
} else{
psps_temp <- read_csv(paste0(intermediate_dir, "ca_zcta_daily_psps_no_washout_2013-2022.csv"))
}

# xwalk zip-zcta
zip_to_zcta_xwalk <- read.csv(paste0(repo, "data/zip_zcta_xref.csv")) %>% 
    mutate(zip_code = str_pad(zip_code, 5, side = "left", pad = 0)) %>%
    select(zip_code, zcta)

# wf data
wf_ca <- read_csv(paste0(raw_dir, "../CAzip_wf_pm25_2006to2020.csv")) %>% 
    mutate(zip_code = str_pad(zip_code, 5, side = "left", pad = 0)) %>%
    left_join(zip_to_zcta_xwalk, by = "zip_code") %>%
    mutate(zcta = str_pad(zcta, 5, side = "left", pad = 0)) %>% 
    select(zcta, date, wf_pm25)

#-------------------------------------------------
# prep wf data
wf_ca_collapse <- wf_ca %>% 
    group_by(zcta, date) %>% 
    summarise(wf_pm25 = mean(wf_pm25, na.rm = TRUE)) %>% 
    ungroup()

#-------------------------------------------------
# step 4: merge on wf data
  # merging on the wf pm from the start date of the fire

psps_analysis <- psps_temp %>% 
  mutate(date = date(outage_start),
          zcta = str_pad(zcta, 5, side = "left", pad = 0)) %>%
  left_join(wf_ca_collapse, by = c("zcta", "date")) %>% 
  mutate(wf_exposed = ifelse(wf_pm25 >= wf_pm_threshold, 1, 0)) %>% 
  select(-c(date, wf_pm25))

#-------------------------------------------------
# clean and write out data

if(washout == TRUE){
  write.csv(psps_analysis, paste0(clean_dir, "ca_zcta_event_level_psps_washout_wf_2013-2022.csv"), row.names = FALSE)
} else{
  write.csv(psps_analysis, paste0(clean_dir, "ca_zcta_event_level_psps_no_washout_wf_2013-2022.csv"), row.names = FALSE)
}
