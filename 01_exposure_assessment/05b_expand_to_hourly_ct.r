#-------------------------------------------------
# PSPS: Expand data back out to hourly (CT)
# author: Lauren Blair Wilner
# updated: April 9, 2025

#-------------------------------------------------
# setup
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(sf, tigris, MetBrewer, lubridate, arrow, raster, tidyverse, scales, knitr, kableExtra, patchwork)

clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
repo <- "~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/"
intermediate_dir <- paste0(repo, "data/intermediate/")

if(washout == TRUE){
  psps_temp <- read_csv(paste0(clean_dir, "ca_ct_event_level_psps_washout_wf_2013-2022.csv"))
} else{
  psps_temp <- read_csv(paste0(clean_dir, "ca_ct_event_level_psps_no_washout_wf_2013-2022.csv"))
}

washout <- FALSE # set to TRUE if you want to apply washout period

#-------------------------------------------------
# step 5: expand out to hourly dataset         
# expand back to hourly dataset

psps_analysis_hourly <- psps_temp %>%
  rowwise() %>%
  mutate(hour = list(seq(from = floor_date(outage_start, "hour"),
                        to = ceiling_date(outage_end, "hour") - hours(1),
                        by = "hour"))) %>%
  unnest(hour) %>%
  ungroup() %>%
  mutate(hours_since_start = as.numeric(difftime(hour, outage_start, units = "hours"))) %>% 
  mutate(hours_since_start = ifelse(hours_since_start < 0, 0, hours_since_start)) %>% 
  select(-c("hour"))


if(washout == TRUE){
    write.csv(psps_analysis_hourly, paste0(clean_dir, "ca_ct_hourly_psps_washout_wf_2013-2022.csv"), row.names = FALSE)
} else{
    write.csv(psps_analysis_hourly, paste0(clean_dir, "ca_ct_hourly_psps_no_washout_wf_2013-2022.csv"), row.names = FALSE)
}