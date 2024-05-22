
#-------------------------------------------------
# PSPS data cleaning: Circuit Level
# March 2024

#-------------------------------------------------
# setup
library(pacman)
p_load(sf, tigris, MetBrewer, lubridate, arrow, raster, tidyverse, scales)

CRS = 'EPSG:3310' # this is california albers

raw_dir <- ("~/Desktop/Desktop/epidemiology_PhD/data/raw/psps_circuit_data/")
raw_raster_dir <- ("~/Desktop/Desktop/epidemiology_PhD/data/raw/")
clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/data/clean/")
plot_dir <- ("~/Desktop/Desktop/epidemiology_PhD/projects/psps/plots/")
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
zcta_shp <- st_transform(zcta_shp, crs(CRS)) # tbd if this is the right direction
zcta_ca <- read_parquet(paste0(clean_dir, "us_ca_zcta_shp.parquet"))
zcta_ca <- st_as_sf(zcta_ca, crs = CRS)

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
# 2. map to zctas (IN PYTHON): 
    # a. map circuits to pixels using CA gridded pop data
    # b. map pixels to zctas
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
`        duration = mean(duration),
`        total_customers_impacted = sum(total_customers_impacted),
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
                row_start = as.POSIXct(row_start,
                    format = "%m/%d/%Y %H:%M",
                    tz = "America/Los_Angeles"),
                row_end = as.POSIXct(row_end,
                    format = "%m/%d/%Y %H:%M",
                    tz = "America/Los_Angeles"))

write.csv(psps_clean, paste0(clean_dir, "psps_underlying_zcta_clean.csv"))



psps_collapse <- psps_clean %>% 
            group_by(psps_event_id, zcta, row_start) %>%
            summarise(duration = sum(duration),
                total_customers_impacted = sum(total_customers_impacted),
                pop = mean(pop),
                customers_out_per_hr = total_customers_impacted/duration,
                pct_cust_out = total_customers_impacted/pop)


# step 3: diagnostics
# top outage events: 
top_events <- psps_collapse %>% 
    group_by(psps_event_id) %>% 
    summarise(total_customers_impacted = sum(total_customers_impacted)) %>% 
    arrange(desc(total_customers_impacted)) %>% 
    head(10)
top_event_ids <- top_events$psps_event_id

top_events_map_df <- psps_collapse %>% 
    filter(psps_event_id %in% top_event_ids) %>% 
    group_by(psps_event_id, zcta) %>% 
    summarise(total_customers_impacted = sum(total_customers_impacted),
            pop = mean(pop)) %>% 
    ungroup() %>% 
    mutate(pct_cust_out = total_customers_impacted/pop) %>% 
    left_join(zcta_shp, by = c("zcta")) %>% 
    st_as_sf()

# plot top events on map
map <- ggplot() + 
    geom_sf(data = zcta_ca, fill = "white", color = "gray") + 
    geom_sf(data = top_events_map_df, aes(fill = total_customers_impacted), color = "black") + 
    scale_fill_viridis_c() + 
    labs(title = "Top 10 PSPS Events by ZCTA", 
        fill = "Total Customers Impacted") + 
    theme_void()
map_pct <- ggplot() + 
    geom_sf(data = zcta_ca, fill = "white", color = "gray") + 
    geom_sf(data = top_events_map_df %>% filter(pct_cust_out <=100), aes(fill = pct_cust_out), color = "black") + 
    scale_fill_viridis_c() + 
    labs(title = "Top 10 PSPS Events by ZCTA", 
        fill = "Percent Customers Impacted") + 
    theme_void()

hist <- ggplot(top_events_map_df, aes(x = total_customers_impacted)) + 
    geom_histogram(bins = 100, fill = "light blue") + 
    labs(title = "Histogram of Total Customers Impacted (by event-zcta)", 
        x = "Total Customers Impacted", 
        y = "Frequency") + 
    theme_bw()

hist_pct <- ggplot(top_events_map_df %>% filter(pct_cust_out<=100), aes(x = pct_cust_out)) + 
    geom_histogram(bins = 100, fill = "light blue") + 
    labs(title = "Histogram of Percent Customers Impacted (by event-zcta)", 
        x = "Percent Customers Impacted", 
        y = "Frequency") + 
    theme_bw()

pdf(paste0(plot_dir, "top_psps_events_plots.pdf"), width = 11, height = 8.5)
hist + hist_pct
map + map_pct
dev.off()

# outstanding issues 
# how to collapse
    # what to do about rows with more than 100% out due to sub event handlign during collapse 
# how to split rows that span multiple days
















# step 3: estimate the number of hours in every 24 hour period (day) where {x}% or more of the customers were without power.
    # we will use a data driven threshold (25%, 50%, etc.)
    # look at the distribution of percents and pick a reasonable cutpoint.
    # count up # of hours that have more than that % off
    # if we want a binary cut for a synthetic control, that can be based on the distribution
# thresholds <- c(0.25, 0.5, 0.75)
# threshold <- 0.25
# # for(threshold in thresholds){
# psps_clean <- psps_clean %>% 
#     mutate(row_hr = ((as.numeric(row_end) - as.numeric(row_start))/60/60), # this is to account for rows that are less than 1 hr at the end of an event
#         customers_out_per_hr = total_customers_impacted/row_hr,
#         pct_cust_out = customers_out_per_hr/pop,
#         pct_out_over_thresh = ifelse(pct_cust_out > threshold, 1, 0),
#         day = as_date(row_start))  # need to figure out how to split a row that spans 2 days 

# # calculate the number of hours in psps_event_id - day where {x}% or more of the customers were without power
# psps_collapse_by_day <- psps_clean %>% 
#     group_by(psps_event_id, day, zcta, row_start) %>%
#     summarise(duration = max(duration), 
#             total_customers_impacted = max(total_customers_impacted),
#             pct_out_over_thresh = max(pct_out_over_thresh)) %>% 
#     unique() %>% 
#     group_by(psps_event_id, day, zcta) %>% 
#     summarise(hrs_with_high_cust_out = sum(pct_out_over_thresh)) %>% 
#     ungroup()


# create histogram of pct_out value with vertical line at x% threshold using ggplot
# ggplot(psps_collapse_by_day, aes(x = hrs_with_high_cust_out)) + 
#     geom_histogram(bins = 100, fill = "light blue") + 
#     labs(title = paste("Histogram of hours per day with more than ", threshold*100, "% of customers out"), 
#         x = paste0("Hours per day with more than ", threshold*100, "% of customers out"), 
#         y = "Frequency") + 
#     # geom_vline(xintercept = threshold*100, color = "red", size = 2) + 
#     theme_bw()
# }

