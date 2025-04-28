## PSPS and Wildfire Smoke ##
## Notes: Processed HCAI data using "HCAI - OOI and t-s controls.R"
## Caitlin Jones-Ngo, MS, PhD, 3/27/25

library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(data.table) 

indir <- ""

## Data import--please check description of each dataset
ooi_long <- fread(file.path(indir, "HCAI_ccts_all_2013_2019.csv")) ## file containing health data for each zip_code-day
psps_levels <- read_csv(file.path(indir, "ca_ZIP_daily_psps_no_washout_classified_2013-2022.csv")) ## file containing psps event data
z_wfpm <- fread(file.path(indir, "CAzip_wfpm25_2006to2020.csv"))  ## file containing wildfire specific pm2.5 concentration for each zip_code-day
z_temp <- fread(file.path(indir, "CAzip_tmmx_90to23.csv")) ## list of zip_code in California

## (1) Prepare exposure data

# Transform PSPS events into daily records from date-time to link to daily health data
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

# apply the function to each PSPS event
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

# assign daily PSPS values for all zip_code-days in study
dates <- seq.Date(as.Date("2013-01-01"), as.Date("2019-12-31"), by = "day")
ca_z <- unique(z_temp$zip)
zip_days <- expand.grid(date = dates, zip_code = ca_z)
zip_days$zip_code <- as.character(zip_days$zip_code) ## without this, warning message: Warning message: In `[<-.factor`(`*tmp*`, ri, value = c(90264, 90264, 90264, 90264,  :invalid factor level, NA generated

psps_daily <- merge(zip_days, daily_exposure, by = c("date","zip_code"), all.x = TRUE)
psps_daily <- as.data.table(psps_daily)
psps_daily[, zip_code := as.character(zip_code)]

# Format PSPS exposure 
psps_daily[is.na(severity_customers), severity_customers := "none"]
psps_daily[is.na(severity_hybrid), severity_hybrid := "none"]
psps_daily$severity_customers <- factor(psps_daily$severity_customers, 
                                        levels = c("none", "Mild", "Moderate", "Severe"))
psps_daily$severity_hybrid <- factor(psps_daily$severity_hybrid, 
                                     levels = c("none", "Mild", "Moderate", "Severe"))

# Remove duplicate event days by selecting the maximum severity for each zip-day
psps_daily$severity_customers_numeric <- as.numeric(psps_daily$severity_customers)
psps_daily$severity_hybrid_numeric <- as.numeric(psps_daily$severity_hybrid)
severity_levels <- c("none", "Mild", "Moderate", "Severe")

duplicate_rows <- psps_daily[duplicated(psps_daily[, .(date, zip_code)]) | duplicated(psps_daily[, .(date, zip_code)], fromLast = TRUE)]
setorder(duplicate_rows, date, zip_code)

# Separate severity_customers and severity_hybrid processing for duplicates
dupe_customers <- duplicate_rows[
  , .SD[severity_customers_numeric == max(severity_customers_numeric), .SD[1]], 
  by = .(date, zip_code)
]

dupe_hybrid <- duplicate_rows[
  , .SD[severity_hybrid_numeric == max(severity_hybrid_numeric), .SD[1]], 
  by = .(date, zip_code)
]

dupe_max <- merge(dupe_customers[,c(1:2,4)], dupe_hybrid[,c(1:2,5)], by = c("date", "zip_code"), all = TRUE)
non_duplicates <- psps_daily[!duplicated(psps_daily[, .(date, zip_code)]) & !duplicated(psps_daily[, .(date, zip_code)], fromLast = TRUE)]

psps_daily_max <- rbind(non_duplicates[,c(1:2,4:5)], dupe_max)

psps_daily_max$severity_customers <- factor(psps_daily_max$severity_customers, levels = severity_levels)
psps_daily_max$severity_hybrid <- factor(psps_daily_max$severity_hybrid, levels = severity_levels)

psps_daily_max <- psps_daily_max %>%
  group_by(zip_code) %>%
  arrange(zip_code, date) %>%
  mutate(
    psps_abs_lag1 = lag(severity_customers, 1),
    psps_hyb_lag1 = lag(severity_hybrid, 1),
    psps_abs_lag2 = lag(severity_customers, 2),
    psps_hyb_lag2 = lag(severity_hybrid, 2)
  ) %>%
  ungroup()

# TEMPERATURE #
z_temps <- z_temp %>%
  filter(year(date) >= 2013 & year(date) <= 2019)

# WFS exposure lags
z_wfpms <- z_wfpm %>%
  filter(date >= "2012-12-25" & year(date) <= 2019)

z_wfpms <- z_wfpms %>%
  group_by(zip_code) %>%
  arrange(date) %>%
  mutate(
    wf = ifelse(wf_pm25 > 15, 1, 0),
    wf_lag1 = as.numeric(lag(wf_pm25, 1)),
    wf_lag2 = as.numeric(lag(wf_pm25, 2)),
    wf_lag3 = as.numeric(lag(wf_pm25, 3)),
    wf_lag4 = as.numeric(lag(wf_pm25, 4)),
    wf_lag5 = as.numeric(lag(wf_pm25, 5))
  ) %>%
  ungroup() %>%
  mutate(
    mean_lag0_lag5 = rowMeans(select(., wf_pm25, wf_lag1, wf_lag2, wf_lag3, wf_lag4, wf_lag5)
                              , na.rm = TRUE)) %>%
  mutate(
    mean_lag05_per10 = mean_lag0_lag5/10)


## (2) Assign exposure and outcome for analysis
# CA ZIP ONLY#
ooi_ca <- ooi_long %>% filter(patzip %in% ca_z) # remove patients residing outside of CA

# Combine outcome and exposure #
ooi_psps <- merge(ooi_ca, psps_daily_max, by.x = c("patzip", "serv_dt"), by.y = c("zip_code", "date"), all.x = TRUE)
ooi_psps_Wf <- merge(ooi_psps, z_wfpms, by.x = c("patzip", "serv_dt"), by.y = c("zip_code", "date"), all.x = TRUE)
ooi_psps_Wft <- merge(ooi_psps_Wf, z_temps, by.x = c("patzip", "serv_dt"), by.y = c("zip", "date"), all.x = TRUE)

# checks; no_ccid <- ooi_psps[is.na(cc_id), ]
# no_psps <- ooi_psps[is.na(severity_hybrid), ]
# invalid_cc_ids <- ooi_psps_Wf[is.na(wf_lag5), .(cc_id)]
# invalid_zip_t <- ooi_psps_Wft[is.na(tmmx_C), .(patzip)]


# age formatting
ooi_psps_Wft[, agecat := fcase(
  agecat %in% c(1:5), "19 years and under", ## I fixed the age categories here according to the 2019 & 2015 PDD/EDD Data Dictionary downloaded from HCAI
  agecat %in% c(6:11), "20-49 years", ## I fixed the age categories here according to the 2019 & 2015 PDD/EDD Data Dictionary downloaded from HCAI
  agecat %in% c(12:14), "50-64 years",
  agecat %in% c(15:17), "65-79 years",
  agecat %in% c(18:19), "80 years and older",
  default = NA_character_
)]

adult_only <- ooi_psps_Wft[agecat != "19 years and under"]

##temporary codes, please delete before finalization--kept here for record only
# zctalist <- unique(adult_only$patzip)
# write.csv(zctalist, file.path(outdir1, "list_zcta_in_analysis.csv"), row.names = F)
##temporary codes, please delete before finalization--kept here for record only

# outcomes of interest (OOI)
adult_cardio <- adult_only[OOI == "cardio"]

adult_resp <- adult_only[OOI == "resp"]

copd_codes <- c("J41", "J42", "J43", "J44",
                "491", "492", "496")
adult_copd <- adult_only[substr(diag_p, 1, 3) %in% copd_codes]

adult_psych <- adult_only[OOI == "psych"]


## (3) Run conditional logistic regression models for PSPS exposure on each OOI, modified by wildfire smoke exposure

library(survival)
library(splines)

dat <- list(adult_resp, adult_copd, adult_cardio, adult_psych)
df_name <- c("adult_resp", "adult_copd", "adult_cardio", "adult_psych")

# Loop through each dataset
for (i in 1:length(dat)) {
  df <- dat[[i]]
  
  # Fit the models
  absmodel <- clogit(case_indicator ~ severity_customers + mean_lag05_per10 + ns(tmmx_C, df = 3) + severity_customers*mean_lag05_per10
                     + strata(cc_id), data = df)
  
  hybmodel <- clogit(case_indicator ~ severity_hybrid + mean_lag05_per10 + ns(tmmx_C, df = 3) + severity_hybrid*mean_lag05_per10
                     + strata(cc_id), data = df)
  
  # Absmodel results
  coef_summary <- as.data.frame(summary(absmodel)["coefficients"])
  or <- exp(coef_summary$coefficients.coef)
  ci <- exp(confint(absmodel))
  p <- coef_summary$coefficients.Pr...z..
  exposures <- rownames(coef_summary)
  
  absresults <-  data.frame(
    Exposure = exposures,
    OR = or,
    CI_Lower = ci[, 1],
    CI_Upper = ci[, 2],
    p = p,
    stringsAsFactors = FALSE)
  
  # Hybmodel results
  coef_summary <- as.data.frame(summary(hybmodel)["coefficients"])
  or <- exp(coef_summary$coefficients.coef)
  ci <- exp(confint(hybmodel))
  p <- coef_summary$coefficients.Pr...z..
  exposures <- rownames(coef_summary)
  
  hybresults <-  data.frame(
    Exposure = exposures,
    OR = or,
    CI_Lower = ci[, 1],
    CI_Upper = ci[, 2],
    p = p,
    stringsAsFactors = FALSE)
  
  # Combine results
  results <- rbind(absresults, hybresults)
  rm(df)
  
  # Save each model output
  assign(paste0("absmodel_", df_name[i]), absmodel)
  assign(paste0("hybmodel_", df_name[i]), hybmodel)
  
  # Assign the result to a variable named results_<dataset_name>
  assign(paste0("results3_", df_name[i]), results)
}