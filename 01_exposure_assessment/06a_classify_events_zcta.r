#-------------------------------------------------
# PSPS: Classify PSPS events by severity (ZCTA)
# author: Lauren Blair Wilner
# updated: April 9, 2025

#-------------------------------------------------
# setup
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(sf, tigris, MetBrewer, lubridate, arrow, raster, tidyverse, scales, knitr, kableExtra, patchwork)

intermediate_dir <- paste0(repo, "data/intermediate/")
clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
repo <- "~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/"

# psps <- read.csv(paste0(clean_dir, "ca_zcta_hourly_psps_no_washout_wf_2013-2022.csv"))
psps <- read.csv(paste0(intermediate_dir, "ca_zcta_daily_psps_no_washout_wf_2013-2022.csv"))

#-------------------------------------------------
# classify events: mild, moderate, severe
# NOTE: only doing this for the no washout data because that is what we use in this particular analysis, which we need classifications for. 
# calculate quantiles
  quantiles_customers <- quantile(psps$total_customers_impacted, probs = c(.33,.66), na.rm=TRUE)
  quantiles_hybrid <- quantile(psps$hybrid, probs = c(.33,.66), na.rm=TRUE)
# choose cutoffs by using interpretable values near the tertile values: 
  quantiles_customers <- c(50,500)
  quantiles_hybrid <- c(1, 150)
  
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

write.csv(psps_class, paste0(clean_dir, 'ca_zcta_event_level_psps_no_washout_wf_classified_2013-2022.csv'), row.names = FALSE)
