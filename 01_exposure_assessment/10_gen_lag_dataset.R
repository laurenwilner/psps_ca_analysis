#-------------------------------------------------
# PSPS: generate lag dataset
# author: Lauren Blair Wilner
# January 2026
#-------------------------------------------------

#-------------------------------------------------
# setup
library(tidyverse)
library(lubridate)
library(patchwork)
clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
plot_dir <- ("~/Desktop/")

df_temp <- read_csv(paste0(clean_dir, 'ca_ZIP_daily_psps_no_washout_wf_classified_2013-2022.csv'))

#-------------------------------------------------
# helper function
assign_severity <- function(x) {
  tertiles <- quantile(x, probs = c(0.33, 0.66), na.rm = TRUE)
  
  # Round to interpretable values
  tertiles <- case_when(
    tertiles > 1 ~ round(tertiles, 0),
    TRUE ~ round(tertiles, 1)
  ) %>% as.numeric()
  
  case_when(
    x < tertiles[1] ~ "mild",
    x < tertiles[2] ~ "moderate",
    TRUE ~ "severe"
  )
}

#-------------------------------------------------
# create the two datasets of interest 

# the first dataset should have the sum of the total_customers_impacted for that zip code-date (so if there are multiple events on the same zip-day, they should be summed) and the average value of hybrid (same thing -- if there are multiple events on the same zip-day, they should be averaged). that dataset should then take the tertiles of each of those two variables, and create severity_customers where that is assigned to mild if total_customers_impacted lies below the first tertile, moderate if it is between the first and second tertile, and severe if it is greater than the third tertile. that dataset should then take the tertiles of each of those two variables, and create severity_hybrid where that is assigned to mild if hybrid lies below the first tertile, moderate if it is between the first and second tertile, and severe if it is greater than the third tertile.  

# the second dataset is built off the first one and should have the average total_customers_impacted where that is an average of total_customers_impacted on the date of that row plus the three days prior. for example, if total_customers_impacted for zip code 98107 on march 10 was 100, march 11 was 110, march 12 was 120, and march 13 was 110, then the value for the row containing zip code 98107 and date=march 12 would be 110 since it is the average of the day of interest plus the three days prior. you can call this new var total_customers_impacted_lag4. The same thing will happen for hybrid. then the same tertile operation should occur in this dataset. you can add _lag4 to each var in this dataset. 

# questions: 
# 1 for the hybrid variable on days with no PSPS event, should those count as 0 in the rolling average, or should they be excluded? currently treating them as NA (excluded). when we then take the average, do we leave these NAs out or what should we do? ANSWER: COUNT THESE AS 0'S NOT NA'S AND INCLUDE THEM IN THE AVERAGE. 
# 2 for the tertile cutoffs, should the severity categories be computed based on: (1) all zip-date combinations (current approach), or (2) only zip-dates that actually had PSPS events. ANSWER: any day that has a 0 average is excluded from the tertile calculation. 


#-------------------------------------------------
# DATASET 1: daily zip code-level sums/averages with tertile severity
# break up each event into daily records for each day it covers
df_temp <- df_temp %>%
  rowwise() %>%
  mutate(date = list(seq.Date(as.Date(outage_start), as.Date(outage_end), by = "day"))) %>%
  ungroup() %>%
  unnest(date)

# create dataset 1: sum of customers, average of hybrid by zip-date
dataset1 <- df_temp %>%
  group_by(zip_code, date) %>%
  summarise(
    total_customers_impacted = sum(total_customers_impacted, na.rm = TRUE),
    hybrid = mean(hybrid, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    severity_customers = assign_severity(total_customers_impacted),
    severity_hybrid = assign_severity(hybrid)
  )

#-------------------------------------------------
# DATASET 2: 4-day rolling average (day of + 3 prior) with tertile severity

# create the base daily data (same as dataset1 before tertiles)
daily_data <- dataset1 %>% select(zip_code, date, total_customers_impacted, hybrid)

# expand to complete grid (fill missing with 0 for customers, NA for hybrid)
complete_grid <- daily_data %>%
  group_by(zip_code) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  mutate(
    total_customers_impacted = replace_na(total_customers_impacted, 0),
    hybrid = replace_na(hybrid, 0)
  ) %>%
  ungroup()

# calculate 4-day rolling average (current day + 3 prior days)
dataset2 <- complete_grid %>%
  arrange(zip_code, date) %>%
  group_by(zip_code) %>%
  mutate(
    total_customers_impacted_lag4 = (
      total_customers_impacted +
      lag(total_customers_impacted, 1, default = 0) +
      lag(total_customers_impacted, 2, default = 0) +
      lag(total_customers_impacted, 3, default = 0)
    ) / 4,
    hybrid_lag4 = (
      hybrid +
      lag(hybrid, 1, default = 0) +
      lag(hybrid, 2, default = 0) +
      lag(hybrid, 3, default = 0)
    ) / 4
  ) %>%
  ungroup() %>%
  # remove the first 3 days per zip code since they don't have full 4-day window
  group_by(zip_code) %>%
  slice(-(1:3)) %>%
  ungroup() %>%
  # filter to only zip days with events
  filter(total_customers_impacted_lag4 > 0) %>%
  # apply tertile severity classifications
  mutate(
    severity_customers_lag4 = assign_severity(total_customers_impacted_lag4),
    severity_hybrid_lag4 = assign_severity(hybrid_lag4)
  ) %>%
  # select final columns
  select(zip_code, date, total_customers_impacted_lag4, hybrid_lag4, 
         severity_customers_lag4, severity_hybrid_lag4)

#-------------------------------------------------
# compile results

cat("dataset 1 (daily sums/averages with severity):\n")
print(dataset1)

cat("\n\ndataset 2 (4-day rolling averages with severity):\n")
print(dataset2)

# check tertile distributions
cat("\n\ndataset 1 - event severity distributions:\n")
cat("total_customers_impacted:\n")
print(table(dataset1$severity_customers))
cat("\nhybrid:\n")
print(table(dataset1$severity_hybrid))

cat("\n\ndataset 2 - event severity distributions:\n")
cat("total_customers_impacted_lag4:\n")
print(table(dataset2$severity_customers_lag4))
cat("\nhybrid_lag4:\n")
print(table(dataset2$severity_hybrid_lag4, useNA = "ifany"))

# four panel-ed histograms of the four variables
# i guess log space?
p1 <- ggplot(dataset1, aes(x = total_customers_impacted, fill = severity_customers)) +
  geom_histogram(bins = 50) +
  labs(title = "total_customers_impacted",
       x = "total_customers_impacted",
       y = "count (log scale)") +
  scale_y_log10() +
  theme_minimal()
p2 <- ggplot(dataset1, aes(x = hybrid, fill = severity_hybrid)) +
  geom_histogram(bins = 50) +
  labs(title = "hybrid",
       x = "hybrid",
       y = "count (log scale)") +
  scale_y_log10() +
  theme_minimal()
p3 <- ggplot(dataset2, aes(x = total_customers_impacted_lag4, fill = severity_customers_lag4)) +
  geom_histogram(bins = 50) +
  labs(title = "total_customers_impacted_lag4",
       x = "total_customers_impacted_lag4",
       y = "count (log scale)") +
  scale_y_log10() +
  theme_minimal()
p4 <- ggplot(dataset2, aes(x = hybrid_lag4, fill = severity_hybrid_lag4)) +
  geom_histogram(bins = 50) +
  labs(title = "hybrid_lag4",
       x = "hybrid_lag4",
       y = "count (log scale)") +
  scale_y_log10() +
  theme_minimal()
p <- p1 + p2 + p3 + p4
p

# write out plots and data
ggsave(paste0(plot_dir, "dataset1_histograms.png"), p, width = 10, height = 10)
write_csv(dataset1, paste0(clean_dir, "ca_ZIP_daily_psps_no_washout_wf_classified_2013-2022_same_day_exp.csv"))
write_csv(dataset2, paste0(clean_dir, "ca_ZIP_daily_psps_no_washout_wf_classified_2013-2022_lag4_exp.csv"))

# check dataset2 for november for 90264
# nov_df <- dataset2 %>%
#   filter(zip_code == 90264, date >= as_date("2019-11-01"), date <= as_date("2019-11-30"))
# print(nov_df)