#-------------------------------------------------
# PSPS: Classify events by severity (zcta)
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
psps_file_name <- 'ca_zcta_daily_psps_no_washout_2013-2022.csv'

psps_temp <- read.csv(paste0(clean_dir, psps_file_name))

#-------------------------------------------------
# classify events: mild, moderate, severe
psps_class <- psps %>%
  mutate(
    severity_customers = case_when(
      total_customers_impacted <= quantiles_customers[1] ~ "Mild",
      total_customers_impacted <= quantiles_customers[2] ~ "Moderate",
      TRUE ~ "Severe"
    ),
    severity_hybrid = case_when(
      hybrid <= quantiles_hybrid[1] ~ "Mild",
      hybrid <= quantiles_hybrid[2] ~ "Moderate",
      TRUE ~ "Severe"
    )
  )

write.csv(psps_class, paste0(clean_dir, 'ca_zcta_daily_psps_no_washout_classified_2013-2022.csv'), row.names = FALSE)
