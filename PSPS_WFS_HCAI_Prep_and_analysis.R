## PSPS and Wildfire Smoke ##
## Notes: Processed HCAI data using "HCAI - OOI and t-s controls.R"
## Caitlin Jones-Ngo, MS, PhD, 2/28/25

library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(data.table) 


## (1) Prepare exposure data

# Transform PSPS events into daily records from date-time to link to daily health data
daily_psps <- function(start_datetime, end_datetime, event_id, zcta, severity_customers, severity_hybrid) {
  # Extract dates
  start_date <- as.Date(start_datetime)
  end_date <- as.Date(end_datetime)
  data.table(
    date = seq.Date(start_date, end_date, by = "day"),
    psps_event_id = event_id,
    zcta = zcta,
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
      psps_levels$zcta[i],
      psps_levels$severity_customers[i],
      psps_levels$severity_hybrid[i]
    )
  }))

# assign daily PSPS values for all zcta-days in study
dates <- seq.Date(as.Date("2013-01-01"), as.Date("2019-12-31"), by = "day")
zcta <- unique(daily_exposure$zcta)
zcta_days <- expand.grid(date = dates, zcta = zcta)

psps_daily <- merge(zcta_days, daily_exposure, by = c("date","zcta"), all.x = TRUE)
psps_daily <- as.data.table(psps_daily)
psps_daily[, zcta := as.character(zcta)]

# fill non-event days with zero
psps_daily[is.na(severity_customers), severity_customers := 0]
psps_daily[is.na(severity_hybrid), severity_hybrid := 0]

# PSPS exposure lags
psps_daily <- psps_daily %>%
  group_by(zcta) %>%
  arrange(zcta,date) %>%
  mutate(
    psps_abs_lag1 = lag(severity_customers, 1),
    psps_hyb_lag1 = lag(severity_hybrid, 1),
    psps_abs_lag2 = lag(severity_customers, 2),
    psps_hyb_lag2 = lag(severity_hybrid, 2)
  ) %>%
  ungroup()

# WFS exposure lags
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
# Combine outcome and exposure
ooi_psps <- merge(ooi_long, psps_daily, by.x = c("patzip", "serv_dt"), by.y = c("zcta", "date"), all.x = TRUE)
ooi_psps_Wf <- merge(ooi_psps, z_wfpms, by.x = c("patzip", "serv_dt"), by.y = c("zip_code", "date"), all.x = TRUE)
ooi_psps_Wft <- merge(ooi_psps_Wf, z_temps, by.x = c("patzip", "serv_dt"), by.y = c("zip", "date"), all.x = TRUE)

# check missingness
invalid_cc_ids <- ooi_psps_Wft[is.na(wf_lag5), .(cc_id)]
ooi_psps_Wft <- ooi_psps_Wft[!cc_id %in% invalid_cc_ids$cc_id]
invalid_zip_t <- ooi_psps_Wft[is.na(tmmx_C), .(patzip)]
invalid_zip_t <- unique(invalid_zip_t)
ooi_psps_Wft <- ooi_psps_Wft[!patzip %in% invalid_zip_t$patzip]

# age formatting
ooi_psps_Wft[, agecat := fcase(
  agecat %in% c(1:9), "19 years and under",
  agecat %in% c(10:11), "20-49 years",
  agecat %in% c(12:14), "50-64 years",
  agecat %in% c(15:17), "65-79 years",
  agecat %in% c(18:19), "80 years and older",
  default = NA_character_
)]

adult_only <- ooi_psps_Wft[agecat != "19 years and under"]

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
  absmodel <- clogit(case_indicator ~ severity_customers + mean_lag05_per10 + ns(tmmx_C, df = 4) + severity_customers*mean_lag05_per10
                     + strata(cc_id), data = df)
  
  hybmodel <- clogit(case_indicator ~ severity_hybrid + mean_lag05_per10 + ns(tmmx_C, df = 4) + severity_hybrid*mean_lag05_per10
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
  # Assign the result to a variable named results_<dataset_name>
  assign(paste0("results2_", df_name[i]), results)
}


## (4) Run models for PSPS Lag 1 effects

dat <- list(adult_resp, adult_copd, adult_cardio, adult_psych)
df_name <- c("adult_resp", "adult_copd", "adult_cardio", "adult_psych")

# Loop through each dataset
for (i in 1:length(dat)) {
  df <- dat[[i]]
  
  # Fit the models
  absmodel <- clogit(case_indicator ~ psps_abs_lag1 + mean_lag05_per10 + ns(tmmx_C, df = 4) + psps_abs_lag1*mean_lag05_per10
                     + strata(cc_id), data = df)
  
  hybmodel <- clogit(case_indicator ~ psps_hyb_lag1 + mean_lag05_per10 + ns(tmmx_C, df = 4) + psps_hyb_lag1*mean_lag05_per10
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
  # Assign the result to a variable named results_<dataset_name>
  assign(paste0("results_pspsl1_", df_name[i]), results)
}


## (5) Run models for PSPS Lag 2 effects
dat <- list(adult_resp, adult_copd, adult_cardio, adult_psych)
df_name <- c("adult_resp", "adult_copd", "adult_cardio", "adult_psych")

# Loop through each dataset
for (i in 1:length(dat)) {
  df <- dat[[i]]
  
  # Fit the models
  absmodel <- clogit(case_indicator ~ psps_abs_lag2 + mean_lag05_per10 + ns(tmmx_C, df = 4) + psps_abs_lag2*mean_lag05_per10
                     + strata(cc_id), data = df)
  
  hybmodel <- clogit(case_indicator ~ psps_hyb_lag2 + mean_lag05_per10 + ns(tmmx_C, df = 4) + psps_hyb_lag2*mean_lag05_per10
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
  # Assign the result to a variable named results_<dataset_name>
  assign(paste0("results_pspsl2_", df_name[i]), results)
}
