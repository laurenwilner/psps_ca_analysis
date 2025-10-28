## PSPS Data Prep V5 ##
## Updates: Reprocessed HCAI data with additional years and OOI (using "HCAI - OOI and t-s controls.R")
## Caitlin Jones-Ngo, MS, PhD, 12/3/24

library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(data.table) 

# Data Import
resp1319 <- fread("D:/OSHPD/Caitlin/CC TS/HCAI_ccts_resp_2013_2019.csv")
cardio1319 <- fread("D:/OSHPD/Caitlin/CC TS/HCAI_ccts_cardio_2013_2019.csv")
psych1319 <- fread("D:/OSHPD/Caitlin/CC TS/HCAI_ccts_psych_2013_2019.csv")

psps_levels <- read_csv("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/ca_ZIP_daily_psps_no_washout_classified_2013-2022.csv")

z_wfpm <- fread("D:/DATA/data_hub/zcta/Daily_environmental/CA ZIP/CAzip_wfpm25_2006to2020.csv")

# formatting
resp1319$OOI <- "resp"
cardio1319$OOI <- "cardio"
psych1319$OOI <- "psych"
ooi_long <- rbind(resp1319, cardio1319, psych1319)

psps_levels$outage_start <- as.POSIXct(psps_levels$outage_start, format="%Y-%m-%d %H:%M:%S")

z_wfpm$date <- as.Date(z_wfpm$date)
z_wfpm <- z_wfpm %>%
  mutate(zip_code = as.character(zip_code))


### EXPOSURE ###

## prepare PSPS data to link to daily health data
# transform into daily records from date-time
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

# apply the function to each event
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

# Days in 2018-2019
dates <- data.table(date = seq.Date(as.Date("2013-01-01"), as.Date("2019-12-31"), by = "day"))

psps_daily <- left_join(dates, daily_exposure, by = "date") %>% 
  select(c("date", "zip_code", "severity_customers", "severity_hybrid")) %>% 
  mutate(psps_abs = ifelse(is.na(severity_customers), 0, 1)) %>%
  mutate(psps_hybrid = ifelse(is.na(severity_hybrid), 0, 1)) %>% 
  filter(!is.na(zip_code)) %>%
  select(-c("severity_customers", "severity_hybrid"))

write.csv(psps_daily, "~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/data/daily_psps_binary.csv", row.names = FALSE)
# Create binary exposure variables
#daily_exposure_2019[, absolute_exposure := ifelse(type == "absolute space", 1, 0)]
#daily_exposure_2019[, percent_exposure := ifelse(type == "percent space", 1, 0)]
#daily_exposure_2019[, hybrid_exposure := ifelse(type == "hybrid", 1, 0)]



# WILDFIRE PM #
z_wfpms <- z_wfpm %>%
  filter(year(date) >= 2013 & year(date) <= 2019)

z_wfpms <- z_wfpms %>%
  group_by(zip_code) %>%
  arrange(date) %>%
  mutate(wf = ifelse(wf_pm25 > 15, 1, 0)) %>%
  ungroup()

# WF exposure lags
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


### ANALYSIS ###
# Combine outcome and exposure #
ooi_psps <- merge(ooi_long, psps_daily, by.x = c("patzip", "serv_dt"), by.y = c("zcta", "date"), all.x = TRUE)
ooi_psps_Wf <- merge(ooi_psps, z_wfpms, by.x = c("patzip", "serv_dt"), by.y = c("zip_code", "date"), all.x = TRUE)

invalid_cc_ids <- ooi_psps_Wf[is.na(wf_lag5), .(cc_id)]
ooi_psps_Wf <- ooi_psps_Wf[!cc_id %in% invalid_cc_ids$cc_id]
ooi_psps_Wf[is.na(severity_customers), severity_customers := 0]
ooi_psps_Wf[is.na(severity_hybrid), severity_hybrid := 0]

# Formatting #
ooi_psps_Wf[, agecat := fcase(
  agecat %in% c(1:9), "19 years and under",
  agecat %in% c(10:11), "20-49 years",
  agecat %in% c(12:14), "50-64 years",
  agecat %in% c(15:17), "65-79 years",
  agecat %in% c(18:19), "80 years and older",
  default = NA_character_
)]

adult_only <- ooi_psps_Wf[agecat != "19 years and under"]

adult_cardio <- adult_only[OOI == "cardio"]

adult_resp <- adult_only[OOI == "resp"]

copd_codes <- c("J41", "J42", "J43", "J44",
                "491", "492", "496")
adult_copd <- adult_only[substr(diag_p, 1, 3) %in% copd_codes]

adult_psych <- adult_only[OOI == "psych"]

exposure <- adult_only %>%
  group_by(case_indicator) %>%
  summarize(
    severity_customers_count = list(table(severity_customers)),
    severity_hybrid_count = list(table(severity_hybrid)),
    wf_count = sum(wf),
    wf_severity_customers_count = list(table(severity_customers[wf == 1])),
    wf_severity_hybrid_count = list(table(severity_hybrid[wf == 1]))) %>%
  unnest(cols = c(severity_customers_count, severity_hybrid_count, wf_severity_customers_count, wf_severity_hybrid_count)) %>%
  gather(key = "severity_level", value = "count", -case_indicator, -wf_count)


  ## Modeling ##

library(survival)

df <- adult_psych

absmodel <- clogit(case_indicator ~ severity_customers + mean_lag05_per10 + severity_customers*mean_lag05_per10
                   + strata(cc_id), data = df)

hybmodel <- clogit(case_indicator ~ severity_hybrid + mean_lag05_per10 + severity_hybrid*mean_lag05_per10
                   + strata(cc_id), data = df)


coef_summary <- as.data.frame(summary(absmodel)["coefficients"])
or <- exp(coef_summary$coefficients.coef)
ci <- exp(confint(absmodel))
p <- coef_summary$coefficients.Pr...z..
exposures <- rownames(coef_summary)

# Create a data frame with results
absresults <-  data.frame(
  Exposure = exposures,
  OR = or,
  CI_Lower = ci[, 1],
  CI_Upper = ci[, 2],
  p = p,
  stringsAsFactors = FALSE)

coef_summary <- as.data.frame(summary(hybmodel)["coefficients"])
or <- exp(coef_summary$coefficients.coef)
ci <- exp(confint(hybmodel))
p <- coef_summary$coefficients.Pr...z..
exposures <- rownames(coef_summary)

# Create a data frame with results
hybresults <-  data.frame(
  Exposure = exposures,
  OR = or,
  CI_Lower = ci[, 1],
  CI_Upper = ci[, 2],
  p = p,
  stringsAsFactors = FALSE)


results <- rbind(absresults,hybresults)
