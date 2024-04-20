#-------------------------------------------------
# PSPS data cleaning: Circuit Level
# March 2024

#-------------------------------------------------
# setup
library(tidyverse)
library(sf)
library(tigris)
library(MetBrewer)

setwd("~/Desktop/Desktop/epidemiology_PhD/data/raw/psps_circuit_data/")
file_name <- '2023.07.clean_psps_data.csv'

#-------------------------------------------------
# import and clean
psps_circuit <- read_csv(file_name)

#-------------------------------------------------
# helper functions
# Function to convert duration string to hours
duration_to_hours <- function(duration_str) {
  pattern <- "\\s*(\\d+) days?,(\\d+) hrs?,(\\d+) min"
  matches <- str_extract(duration_str, pattern)
  if (!is.na(matches)) {
    components <- strsplit(matches, ",")
    durations <- lapply(components, function(x) {
      parts <- as.numeric(unlist(str_extract_all(x, "\\d+")))
      parts[1] * 24 + parts[2] + parts[3] / 60
    })
    return(unlist(durations))
  } else {
    return(NA)
  }
}
#-------------------------------------------------
# EDA

psps <- psps_circuit %>% 
    select(c(Sub_Event_ID, Outage_Start, Outage_Full_Restoration, Outage_Duration, Total_Customers_Impacted)) %>% 
    mutate(duration = sapply(Outage_Duration, duration_to_hours)) %>% 
    rename_all(tolower)

# histogram of outage_duration
duration_hist <- ggplot(psps, aes(x = duration)) + 
    geom_histogram(bins = 100, fill = "light blue") + 
    labs(title = "Histogram of Outage Duration (hours)", 
        x = "Duration (hours)", 
        y = "Frequency") + 
    theme_bw()

# histogram of total_customers_impacted
customers_hist <- ggplot(psps, aes(x = total_customers_impacted)) + 
    geom_histogram(bins = 100, fill = "lightblue") + 
    labs(title = "Histogram of Total Customers Impacted", 
        x = "Total Customers Impacted", 
        y = "Frequency") + 
    theme_bw()

# histogram of total_customers_impacted > 0
customers_hist_no0 <- ggplot(psps %>% filter(total_customers_impacted > 0), aes(x = total_customers_impacted)) + 
    geom_histogram(bins = 100, fill = "light blue") + 
    labs(title = "Histogram of Total Customers Impacted (>0)", 
        x = "Total Customers Impacted", 
        y = "Frequency") + 
    theme_bw()


#-------------------------------------------------
# EDA

# every row should be a circuit hour. if there is overlap, then just average the two rows. 
# stay in circuit space for as long as possible and then go to zctas at the end. 
    # to get to zcta space, map circuits to pixels using CA gridded pop data and then map pixels to zctas.


# customer hours without power (absolute metric of power loss)
# customers (denominator): households+businesses at zip level
# we will use a data driven threshold (25%, 50%, etc.) 
    # estimate the number of hours in every 24 hour period where {x}% or more of the customers were without power. 
    # look at the distribution of percents and pick a reasonable cutpoint. 
    # count up # of hours that have more than that % off
    # if we want a binary cut for a synthetic control, that can be based on the distribution

