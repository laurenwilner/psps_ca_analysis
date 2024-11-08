#-------------------------------------------------
# PSPS data cleaning: Circuit Level
# March 2024

#-------------------------------------------------
# setup
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(sf, tigris, MetBrewer, lubridate, arrow, raster, tidyverse, scales, knitr, kableExtra, patchwork)

CRS = 'EPSG:3310' # this is california albers

raw_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/raw/psps_circuit_data/")
raw_raster_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/raw/")
clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
plot_dir <- ("~/Desktop/Desktop/epidemiology_PhD/02_projects/psps/plots/")
psps_file_name <- '2023.07.clean_psps_data.csv'

washout <- FALSE
wf_pm_threshold <- 15 # threshold for wildfire pm2.5 but should look at distribution before finalizing

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
cali_zctas <- unique(zcta_ca$zcta)

zip_to_zcta_xwalk <- read_csv(paste0(raw_dir, "../zip_zcta_xref.csv")) %>% 
    select(zip_code, zcta) %>%
    mutate(zip_code = str_pad(zip_code, 5, side = "left", pad = 0), 
            zcta = str_pad(zcta, 5, side = "left", pad = 0))
wf_ca <- read_csv(paste0(raw_dir, "../CAzip_wf_pm25_2006to2020.csv")) %>% 
    mutate(zip_code = str_pad(zip_code, 5, side = "left", pad = 0)) %>%
    left_join(zip_to_zcta_xwalk, by = "zip_code") %>%
    select(zcta, date, wf_pm25)

#-------------------------------------------------
# cleaning

# steps: 
# step 1 occurs in this script 
# 1. make each row a circuit-event-hr (ie sub_event-hr)
    # if there is overlap on times/circuits, average the rows
    # calculate the number of customers impacted in each circuit hour
    # calculate the number of customers hours without power in each sub_event (absolute metric of power loss)

# step 2 occurs in python
# 2. map to zctas (IN PYTHON): 
    # a. map circuits to pixels using CA gridded pop data
    # b. map pixels to zctas

# steps 3-4 occur in 02_data_linkage.R script
# 3. washout period 
    # anything within a week prior to an event gets collapsed to one event. 
    # anything >1 and <4 weeks prior gets excluded.
# 4. merge on wf data
# 5. expand out to hourly dataset
         
# step 6 occurs in 03_classify_events.R script
# 6. classify events as mild/moderate/severe


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
    hourly_seq <- seq(from = .$outage_start, to =    .$outage_end, by = "hour")
    data.frame(
      .,
      row_start = hourly_seq,
      row_end = pmin(hourly_seq + hours(1), .$outage_end)  # make sure that row_end does not exceed outage_end
    )
  }) %>%
  ungroup() %>%
  filter(row_start < outage_end) # if outage start is exactly the end of the outage for a given row, get rid of it. 

# write out file to do this in python
write_parquet(psps_expanded, paste0(clean_dir, "us_circuit_psps_by_hr.parquet"))

