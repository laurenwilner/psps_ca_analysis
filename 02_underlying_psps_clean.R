
#-------------------------------------------------
# PSPS data cleaning: Circuit Level
# March 2024

#-------------------------------------------------
# setup
library(pacman)
p_load(sf, tigris, MetBrewer, lubridate, arrow, raster, tidyverse)

raw_dir <- ("~/Desktop/Desktop/epidemiology_PhD/data/raw/psps_circuit_data/")
raw_raster_dir <- ("~/Desktop/Desktop/epidemiology_PhD/data/raw/")
clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/data/clean/")
psps_file_name <- '2023.07.clean_psps_data.csv'

#-------------------------------------------------
# import
psps_circuit <- read_csv(paste0(raw_dir, psps_file_name))

pge_polyline <- st_read(paste0(raw_dir, "ICA_circuits/PGE/PGE_circuits_lines/PGE_circuits_lines.shp"))
sce_polyline <- st_read(paste0(raw_dir, "ICA_circuits/SCE/SCE_circuits/SCE_circuits.shp"))
sdge_polyline <- st_read(paste0(raw_dir, "ICA_circuits/SDGE_circuits/SDGE_circuits.shp"))

households <- read_parquet(paste0(clean_dir, "us_zcta_households.parquet"))
businesses <- read_parquet(paste0(clean_dir, "us_zcta_cbp.parquet"))

ca_pop_raster <- raster(paste0(raw_raster_dir, "CAPOP_2020_100m_TOTAL.tif"))

zcta_shp <- st_read(paste0(clean_dir, "us_zcta_boundaries.geojson"))
zcta_shp <- st_transform(zcta_shp, crs(ca_pop_raster)) # tbd if this is the right direction

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

psps_temp <- psps_circuit %>% 
    dplyr::select(c(Circuit_Name_ICA, PSPS_Event_ID, Sub_Event_ID, Outage_Start, Outage_Full_Restoration,
            Outage_Duration, Total_Customers_Impacted)) %>% 
    mutate(duration = sapply(Outage_Duration, duration_to_hours)) %>% 
    rename_all(tolower)

# histogram of outage_duration
duration_hist <- ggplot(psps_temp, aes(x = duration)) + 
    geom_histogram(bins = 100, fill = "light blue") + 
    labs(title = "Histogram of Outage Duration (hours)", 
        x = "Duration (hours)", 
        y = "Frequency") + 
    theme_bw()

# histogram of total_customers_impacted
customers_hist <- ggplot(psps_temp, aes(x = total_customers_impacted)) + 
    geom_histogram(bins = 100, fill = "lightblue") + 
    labs(title = "Histogram of Total Customers Impacted", 
        x = "Total Customers Impacted", 
        y = "Frequency") + 
    theme_bw()

# histogram of total_customers_impacted > 0
customers_hist_no0 <- ggplot(psps_temp %>% filter(total_customers_impacted > 0), aes(x = total_customers_impacted)) + 
    geom_histogram(bins = 100, fill = "light blue") + 
    labs(title = "Histogram of Total Customers Impacted (>0)", 
        x = "Total Customers Impacted", 
        y = "Frequency") + 
    theme_bw()


#-------------------------------------------------
# cleaning

# steps: 
# 1. make each row a circuit-event-hr (ie sub_event-hr)
    # if there is overlap on times/circuits, average the rows
    # calculate the number of customers impacted in each circuit hour
    # calculate the number of customers hours without power in each sub_event (absolute metric of power loss)
# 2. map to zctas: 
    # a. map circuits to pixels using CA gridded pop data
    # b. map pixels to zctas
# 3. merge households and businesses at zcta level (denominator = households+businesses at zcta level)
# 4. estimate the number of hours in every 24 hour period where {x}% or more of the customers were without power.
    # we will use a data driven threshold (25%, 50%, etc.) 
    # look at the distribution of percents and pick a reasonable cutpoint. 
    # count up # of hours that have more than that % off
    # if we want a binary cut for a synthetic control, that can be based on the distribution

# step 1: make each row a circuit-event-hr (ie sub_event-hr)
    # if there is overlap on times/circuits, average the rows
psps_hourly <- psps_temp %>%
    mutate(outage_start = as.POSIXct(outage_start, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles"),
        outage_end = as.POSIXct(outage_full_restoration, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")) %>%
    group_by(circuit_name_ica, psps_event_id, sub_event_id) %>%
    summarise(outage_start = as.POSIXct(mean(as.numeric(outage_start))),
        outage_end = as.POSIXct(mean(as.numeric(outage_end))),
        duration = mean(duration),
        total_customers_impacted = sum(total_customers_impacted),
        customers_out_per_hr = total_customers_impacted/duration) %>%
    ungroup()
    
psps_expanded <- psps_hourly %>%
  rowwise() %>%
  do({
    hourly_seq <- seq(from = .$outage_start, to = .$outage_end, by = "hour")
    data.frame(
      .,
      row_start = hourly_seq,
      row_end = pmin(hourly_seq + hours(1), .$outage_end)  # make sure that row_end does not exceed outage_end
    )
  }) %>%
  ungroup() %>%
  filter(row_start < outage_end) # if outage start is exactly the end of the outage for a given row, get rid of it. 

write_parquet(psps_expanded, paste0(clean_dir, "us_circuit_psps_by_hr.parquet"))

# step 2: map to zctas: I think i should do this in python
    # a. merge on polyline data
    # b. get from polylines to pixels
    # c. get from pixels to zctas

# step 3: merge households and businesses at zcta level

pop <- households %>% 
        merge(businesses, by = "zcta") %>%
        mutate(pop = households + businesses) %>% 
        dplyr::select(zcta, pop)
# psps <- psps_zcta %>% 
#     left_join(pop, by = c("zcta" = "zcta"))



# step 4: estimate the number of hours in every 24 hour period where {x}% or more of the customers were without power.
# we will use a data driven threshold (25%, 50%, etc.)
# look at the distribution of percents and pick a reasonable cutpoint.
# count up # of hours that have more than that % off
# if we want a binary cut for a synthetic control, that can be based on the distribution


# Questions
# Step 1:
    # QUESTION: WHAT DO WE WANT TO USE AS THE START/END TIME?
        # currently, i am taking the mean start/end/duration and the sum of customer hours
            # since each customer can only be on one segment so we should sum the customers
            # but if we want average duration then we should average the start/end times so that
            # duration still adds up
    # QUESTION: circuit-event_id should be the same as sub_event_id, but it is not. there is 
        # sometimes `circuit_event_id_X` and more than 1. so, i am grouping by sub_event_id,
        # circuit_name_ica, and psps_event_id - but we should ask bethany next time we chat.
        # not urgent for now.
    # Comment: I am using the customers out per hr as the total_customers_out/duration for a
        # given circuit-event-subevent and then creating a row for every hr and then propogating that var forward.
        # there are also rows that are less than an hr if the outage was not precisely a whole number of hours of outage.
    # Comment:
        # total_customers_impacted is the sum of customers impacted in a given circuit-event-subevent,
            # even though the rows are event-subevent-circuit-hours
# Step 3: 
    # QUESTION: which crs should i use for CA?
