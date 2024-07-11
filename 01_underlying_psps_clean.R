
#-------------------------------------------------
# PSPS data cleaning: Circuit Level
# March 2024

#-------------------------------------------------
# setup
library(pacman)
p_load(sf, tigris, MetBrewer, lubridate, arrow, raster, tidyverse, scales, knitr, kableExtra)

CRS = 'EPSG:3310' # this is california albers

raw_dir <- ("~/Desktop/Desktop/epidemiology_PhD/data/raw/psps_circuit_data/")
raw_raster_dir <- ("~/Desktop/Desktop/epidemiology_PhD/data/raw/")
clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/data/clean/")
plot_dir <- ("~/Desktop/Desktop/epidemiology_PhD/projects/psps/plots/")
psps_file_name <- '2023.07.clean_psps_data.csv'

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
# import
psps_circuit <- read_csv(paste0(raw_dir, psps_file_name))
psps_temp <- psps_circuit %>% 
    dplyr::select(c(Circuit_Name_ICA, PSPS_Event_ID, Sub_Event_ID, Outage_Start, Outage_Full_Restoration,
            Outage_Duration, Total_Customers_Impacted)) %>% 
    mutate(duration = sapply(Outage_Duration, duration_to_hours)) %>% 
    rename_all(tolower)

pge_polyline <- st_read(paste0(raw_dir, "ICA_circuits/PGE/PGE_circuits_lines/PGE_circuits_lines.shp"))
sce_polyline <- st_read(paste0(raw_dir, "ICA_circuits/SCE/SCE_circuits/SCE_circuits.shp"))
sdge_polyline <- st_read(paste0(raw_dir, "ICA_circuits/SDGE_circuits/SDGE_circuits.shp"))

households <- read_parquet(paste0(clean_dir, "us_zcta_households.parquet"))
businesses <- read_parquet(paste0(clean_dir, "us_zcta_cbp.parquet"))

ca_pop_raster <- raster(paste0(raw_raster_dir, "CAPOP_2020_100m_TOTAL.tif"))

zcta_shp <- st_read(paste0(clean_dir, "us_zcta_boundaries.geojson"))
zcta_shp <- st_transform(zcta_shp, crs(CRS))
zcta_ca <- read_parquet(paste0(clean_dir, "us_ca_zcta_shp.parquet"))
zcta_ca <- st_as_sf(zcta_ca, crs = CRS)

#-------------------------------------------------
# EDA

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
# 2. map to zctas (IN PYTHON): 
    # a. map circuits to pixels using CA gridded pop data
    # b. map pixels to zctas
# 3. diagnostics 


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


## TODO: USE TOTAL RESIDENTIAL TO MATCH DENOM AND BC WE CARE ABOTU RESIDENCES NOT BUSINESSES!
    
psps_expanded <- psps_hourly %>%
  rowwise() %>%
  do({
    hourly_seq <- seq(from = .$outage_start, to =    .$outage_end, by = "hour")
    data.frame(
      .,
      row_start = hourly_seq,
      row_end = pmin(hourly_seq + hours(1), .$outage_end)  # make sure that row_end does not exceed outage_end
    )
  }) %>%
  ungroup() %>%
  filter(row_start < outage_end) # if outage start is exactly the end of the outage for a given row, get rid of it. 

# write_parquet(psps_expanded, paste0(clean_dir, "us_circuit_psps_by_hr.parquet"))

# step 2: map to zctas: I think i should do this in python
    # a. merge on polyline data
    # b. get from polylines to pixels
    # c. get from pixels to zctas
# read in file created in python 
denom <- read_parquet(paste0(clean_dir, "ca_gridded_zcta_pop.parquet"))
num <- read_parquet(paste0(clean_dir, "ca_zcta_psps_customers_impacted.parquet"))
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
                pct_cust_out = ifelse(pct_cust_out > 1, 1, pct_cust_out)) # if more than 100% of the population is out, set to 100%


                
# expand back to hourly dataset
psps_clean_hourly <- psps_clean %>%
  rowwise() %>%
  mutate(hour = list(seq(from = floor_date(outage_start, "hour"),
                         to = ceiling_date(outage_end, "hour") - hours(1),
                         by = "hour"))) %>%
  unnest(hour) %>%
  ungroup() %>%
  mutate(hours_since_start = as.numeric(difftime(hour, outage_start, units = "hours"))) %>% 
  mutate(hours_since_start = ifelse(hours_since_start < 0, 0, hours_since_start))

write.csv(psps_clean_hourly, paste0(clean_dir, "psps_underlying_zcta_clean_hourly.csv"))
write.csv(psps_clean, paste0(clean_dir, "psps_underlying_zcta_clean.csv"))

# step 3: diagnostics
# top outage events: 
top_events_abs <- psps_clean %>% 
    arrange(desc(total_customers_impacted)) %>% 
    mutate(hybrid = total_customers_impacted * pct_cust_out,
        type = 'absolute space') %>% 
    head(10)
top_events_pct <- psps_clean %>% 
    filter(pct_cust_out ==1) %>%
    arrange(desc(total_customers_impacted)) %>% 
    mutate(hybrid = total_customers_impacted * pct_cust_out,
            type = 'percent space') %>% 
    head(10)
top_events_hybrid <- psps_clean %>% 
    mutate(hybrid = total_customers_impacted * pct_cust_out,
            type = 'hybrid') %>%
    arrange(desc(hybrid)) %>%
    head(10)

table_df <- rbind(top_events_abs, top_events_pct, top_events_hybrid)

write.csv(table_df, paste0(plot_dir, "top_psps_events.csv"))



# # step 3: aggregate the by day and ZCTA:

# # A. summarize the data by ZCTA and day
# # create variables event_in_last_week and event_in_last_month.
# # filter out the rows where event_in_last_week is 0 and event_in_last_month is 1.

# # agg data by day and ZCTA. sum total customers but average duration
# psps_daily <- psps_clean %>%
#     mutate(day = as.Date(row_start)) %>%
#     group_by(zcta, day) %>%
#     summarise(
#         total_customers_impacted_day = sum(total_customers_impacted),
#         duration_day = mean(duration),
#         pop = first(pop)
#     )

# # create event_in_last_week and event_in_last_month variables
# psps_daily <- psps_daily %>%
#     arrange(zcta, day) %>%
#     group_by(zcta) %>%
#     mutate(
#         event_in_last_week = lag(cumsum(!is.na(day)), 7, order_by = day) > 0,
#         event_in_last_month = lag(cumsum(!is.na(day)), 30, order_by = day) > 0
#     )

# # filter out rows where there was an event in the last month but not the last week
# psps_daily_filtered <- psps_daily %>%
#     filter(!(event_in_last_week != TRUE & event_in_last_month == TRUE))

# # B. plot
# # identify the top 10 ZCTA-days based on total_customers_impacted and duration.
# top_10_zcta_days_cust <- psps_daily_filtered %>%
#     arrange(desc(total_customers_impacted_day)) %>%
#     head(10) %>% 
#     merge(zcta_ca, by = "zcta", all.x = TRUE) %>% 
#     st_as_sf()
# top_10_zcta_days_dur <- psps_daily_filtered %>%
#     arrange(desc(duration_day)) %>%
#     head(10) %>% 
#     merge(zcta_ca, by = "zcta", all.x = TRUE) %>% 
#     st_as_sf()

# # create a map with CA counties as the basemap and the selected ZCTAs filled in based on the variables total_customers_impacted and duration
# map_customers <- ggplot() + 
#     geom_sf(data = zcta_ca, fill = "white", color = "gray") + 
#     geom_sf(data = top_10_zcta_days_cust, aes(fill = total_customers_impacted_day), color = "black") + 
#     scale_fill_viridis_c() + 
#     labs(title = "Top 10 PSPS events by by ZCTA-day: customers impacted", 
#         fill = "customers impacted") + 
#     theme_void()
# map_duration <- ggplot() + 
#     geom_sf(data = zcta_ca, fill = "white", color = "gray") + 
#     geom_sf(data = top_10_zcta_days_dur, aes(fill = duration_day), color = "black") + 
#     scale_fill_viridis_c() + 
#     labs(title = "Top 10 PSPS events by by ZCTA-day: duration", 
#         fill = "duration") + 
#     theme_void()

# map_customers + map_duration





