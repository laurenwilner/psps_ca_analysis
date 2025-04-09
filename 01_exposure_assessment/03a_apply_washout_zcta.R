#-------------------------------------------------
# PSPS: Apply washout period to data if applicable (ZCTA)
# author: Lauren Blair Wilner
# updated: April 9, 2025

#-------------------------------------------------
# setup
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(sf, tigris, MetBrewer, lubridate, arrow, raster, tidyverse, scales, knitr, kableExtra, patchwork)

clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
repo <- "~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/"
intermediate_dir <- paste0(repo, "data/intermediate/")

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

# these files are the outputs of python script called 02_psps_to_zcta_ct.ipynb 
denom <- read_parquet(paste0(clean_dir, "ca_gridded_zcta_pop.parquet"))
num <- read_parquet(paste0(clean_dir, "ca_zcta_psps_customers_impacted.parquet"))

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
                pct_cust_out = ifelse(pct_cust_out > 1, 1, pct_cust_out), # if more than 100% of the population is out, set to 100%
                hybrid = total_customers_impacted * pct_cust_out) # using this hybrid metric, too

# step 3: include/exclude based on washout period: 
    # anything within a week prior to an event gets collapsed to one event. 
    # anything >1 and <4 weeks prior gets excluded.

if(washout == TRUE){
  psps_washout <- collapse_events_rle(psps_clean)
  psps_analysis <- exclude_events_rle(psps_washout)
} else{
  psps_analysis <- psps_clean
}

# write out daily data with/without washout 
if(washout == TRUE){
  write.csv(psps_analysis, paste0(intermediate_dir, "ca_zcta_daily_psps_washout_2013-2022.csv"), row.names = FALSE)
} else{
  write.csv(psps_analysis, paste0(intermediate_dir, "ca_zcta_daily_psps_no_washout_2013-2022.csv"), row.names = FALSE)
}