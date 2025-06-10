#-------------------------------------------------
# PSPS: Crosswalk from ZCTA to Zip Code for analysis
# author: Lauren Blair Wilner
# March 2025

#-------------------------------------------------
# setup
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(tidyverse)

repo_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/")
clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
psps_file_name <- 'ca_zcta_event_level_psps_no_washout_wf_classified_2013-2022.csv'

zip_zcta_xwalk <- read.csv(paste0(repo_dir, 'data/zip_zcta_xref.csv'))
psps_temp <- read.csv(paste0(clean_dir, psps_file_name))

#-------------------------------------------------
psps_zip <- psps_temp %>%
  left_join(zip_zcta_xwalk, by = c('zcta'), relationship = 'many-to-many') %>% 
  dplyr::select(-c('zcta', 'source')) %>% 
  relocate(zip_code, .after = psps_event_id)

write.csv(psps_zip, paste0(clean_dir, 'ca_ZIP_daily_psps_no_washout_wf_classified_2013-2022.csv'), row.names = FALSE)
