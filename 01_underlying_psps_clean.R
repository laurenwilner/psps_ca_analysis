
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

# tract_to_zip_xwalk <- read_csv(paste0(raw_dir, "../ZIP_TRACT_032020.csv")) %>% 
#     select(ZIP, TRACT) %>%
#     mutate(TRACT = str_pad(TRACT, 11, side = "left", pad = 0), 
#             ZIP   = str_pad(ZIP,    5, side = "left", pad = 0)) %>% 
#     rename(zip_code = ZIP, tract = TRACT)

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
# 3. washout period 
    # anything within a week prior to an event gets collapsed to one event. 
    # anything >1 and <4 weeks prior gets excluded.
# 4. merge on wf data
# 5. expand out to hourly dataset         
# 6. diagnostics 


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

# step 2: map to zctas in python
    # a. merge on polyline data
    # b. get from polylines to pixels
    # c. get from pixels to zctas

# write out file to do this in python
    # write_parquet(psps_expanded, paste0(clean_dir, "us_circuit_psps_by_hr.parquet"))

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

# step 3: include/exclude based on washout period: 
    # anything within a week prior to an event gets collapsed to one event. 
    # anything >1 and <4 weeks prior gets excluded.

if(washout == TRUE){
  psps_washout <- collapse_events_rle(psps_clean)
  psps_analysis <- exclude_events_rle(psps_washout)
} else{
  psps_analysis <- psps_clean
}

# step 4: merge on wf data
  # merging on the wf pm from the start date of the fire
# wf_ca <- wf %>% 
#     rename(tract = geoid) %>%
#     mutate(tract = as.character(tract),
#           tract = str_pad(tract, 11, side = "left", pad = 0)) %>% 
#     left_join(tract_to_zip_xwalk, by = "tract") %>% 
#     left_join(zip_to_zcta_xwalk, by = "zip_code") %>% 
#     filter(zcta %in% cali_zctas) %>%
#     group_by(zcta, date) %>% 
#     summarise(mean_wf_pm25 = mean(wf_pm25_aug2023, na.rm = TRUE)) 
  # take pop weighted mean rather than just a mean. so that if there are many tracts per zip, we weight by the percent overlap of each tract with each zip. 
wf_ca_collapse <- wf_ca %>% 
    group_by(zcta, date) %>% 
    summarise(wf_pm25 = mean(wf_pm25, na.rm = TRUE)) %>% 
    ungroup()
psps_analysis <- psps_analysis %>% 
    mutate(date = date(outage_start)) %>%
    left_join(wf_ca_collapse, by = c("zcta", "date")) %>% 
    mutate(wf_exposed = ifelse(wf_pm25 >= wf_pm_threshold, 1, 0)) %>% 
    select(-c(date, wf_pm25))

# step 5: expand out to hourly dataset         
# expand back to hourly dataset
psps_analysis_hourly <- psps_analysis %>%
  rowwise() %>%
  mutate(hour = list(seq(from = floor_date(outage_start, "hour"),
                         to = ceiling_date(outage_end, "hour") - hours(1),
                         by = "hour"))) %>%
  unnest(hour) %>%
  ungroup() %>%
  mutate(hours_since_start = as.numeric(difftime(hour, outage_start, units = "hours"))) %>% 
  mutate(hours_since_start = ifelse(hours_since_start < 0, 0, hours_since_start))

if(washout == TRUE){
  write.csv(psps_analysis_hourly, paste0(clean_dir, "psps_underlying_zcta_clean_hourly_washout.csv"))
  write.csv(psps_analysis, paste0(clean_dir, "psps_underlying_zcta_clean_washout.csv"))
} else{
  write.csv(psps_analysis_hourly, paste0(clean_dir, "psps_underlying_zcta_clean_hourly_no_washout.csv"))
  write.csv(psps_analysis, paste0(clean_dir, "psps_underlying_zcta_clean_no_washout.csv"))
}


# step 6: diagnostics
# top outage events: 
# top_events_abs <- psps_clean %>% 
#     arrange(desc(total_customers_impacted)) %>% 
#     mutate(hybrid = total_customers_impacted * pct_cust_out,
#         type = 'absolute space') %>% 
#     head(10)
# top_events_pct <- psps_clean %>% 
#     filter(pct_cust_out ==1) %>%
#     arrange(desc(total_customers_impacted)) %>% 
#     mutate(hybrid = total_customers_impacted * pct_cust_out,
#             type = 'percent space') %>% 
#     head(10)
# top_events_hybrid <- psps_clean %>% 
#     mutate(hybrid = total_customers_impacted * pct_cust_out,
#             type = 'hybrid') %>%
#     arrange(desc(hybrid)) %>%
#     head(10)
# table_df <- rbind(top_events_abs, top_events_pct, top_events_hybrid)
# write.csv(table_df, paste0(plot_dir, "top_psps_events.csv"))

# pal <- met.brewer("Derain")

# p1 <- ggplot(psps_analysis %>% filter(wf_pm25>0 & !is.na(wf_pm25)),
#   aes(x = wf_pm25)) +
#   geom_histogram(binwidth = 5, fill = pal[1], color = pal[1]) +
#   theme_minimal() +
#   labs(title = "Histogram of wf_pm25: all wf data on PSPS days",
#     x = "pm", y = "frequency",
#     caption = paste0(nrow(psps_analysis %>% filter(wf_pm25>0 & !is.na(wf_pm25))), " days with non-zero PM out of ", nrow(psps_analysis), " total events"))
# p2a <- ggplot(psps_analysis %>% filter(wf_pm25>15 & !is.na(wf_pm25)),
#   aes(x = wf_pm25)) +
#   geom_histogram(binwidth = 2.5, fill = pal[3], color = pal[3]) +
#   theme_minimal() +
#   labs(title = "Histogram of wf_pm25: PM > 15 on PSPS days",
#     x = "pm", y = "frequency",
#     caption = paste0(nrow(psps_analysis %>% filter(wf_pm25>15 & !is.na(wf_pm25))), " days with non-zero PM > 15 out of ", nrow(psps_analysis), " total PSPS days"))
# p2b <- ggplot(psps_analysis %>% filter(wf_pm25<15 & !is.na(wf_pm25) & wf_pm25 > 0),
#   aes(x = wf_pm25)) +
#   geom_histogram(binwidth = 1, fill = pal[2], color = pal[2]) +
#   theme_minimal() +
#   labs(title = "Histogram of wf_pm25: PM < 15 on PSPS days",
#     x = "pm", y = "frequency",
#     caption = paste0(nrow(psps_analysis %>% filter(wf_pm25<15 & !is.na(wf_pm25) & wf_pm25 > 0)), " days with non-zero PM < 15 out of ", nrow(psps_analysis), " total PSPS days"))
# p3a <- ggplot(psps_analysis %>% filter(wf_pm25>25 & !is.na(wf_pm25)),
#   aes(x = wf_pm25)) +
#   geom_histogram(binwidth = 2.5, fill = pal[5], color = pal[5]) +
#   theme_minimal() +
#   labs(title = "Histogram of wf_pm25: PM > 25 on PSPS days",
#     x = "pm", y = "frequency",
#     caption = paste0(nrow(psps_analysis %>% filter(wf_pm25>25 & !is.na(wf_pm25))), " days with non-zero PM > 25 out of ", nrow(psps_analysis), " total PSPS days"))
# p3b <- ggplot(psps_analysis %>% filter(wf_pm25<25 & !is.na(wf_pm25) & wf_pm25 > 0),
#   aes(x = wf_pm25)) +
#   geom_histogram(binwidth = 1, fill = pal[4], color = pal[4]) +
#   theme_minimal() +
#   labs(title = "Histogram of wf_pm25: PM < 25 on PSPS days",
#     x = "pm", y = "frequency",
#     caption = paste0(nrow(psps_analysis %>% filter(wf_pm25<25 & !is.na(wf_pm25) & wf_pm25 > 0)), " days with non-zero PM < 25 out of ", nrow(psps_analysis), " total PSPS days"))
# p4a <- ggplot(psps_analysis %>% filter(wf_pm25>35 & !is.na(wf_pm25)),
#   aes(x = wf_pm25)) +
#   geom_histogram(binwidth = 2.5, fill = pal[7], color = pal[7]) +
#   theme_minimal() +
#   labs(title = "Histogram of wf_pm25: PM > 35 on PSPS days",
#     x = "pm", y = "frequency",
#     caption = paste0(nrow(psps_analysis %>% filter(wf_pm25>35 & !is.na(wf_pm25))), " days with non-zero PM > 35 out of ", nrow(psps_analysis), " total PSPS days"))
# p4b <- ggplot(psps_analysis %>% filter(wf_pm25<35 & !is.na(wf_pm25) & wf_pm25 > 0),
#   aes(x = wf_pm25)) +
#   geom_histogram(binwidth = 1, fill = pal[6], color = pal[6]) +
#   theme_minimal() +
#   labs(title = "Histogram of wf_pm25: PM < 35 on PSPS days",
#     x = "pm", y = "frequency",
#     caption = paste0(nrow(psps_analysis %>% filter(wf_pm25<35 & !is.na(wf_pm25) & wf_pm25 > 0)), " days with non-zero PM < 35 out of ", nrow(psps_analysis), " total PSPS days"))

# p1 + (p2b/p3b/p4b) + (p2a/p3a/p4a) + plot_layout(ncol = 3, nrow = 1)




ics_209_2000_2019_link_to_all_cplx_data_distinct <- ics_209_2000_2019_link_to_all_cplx_data_distinct %>%
  mutate(across(
    c(
      list_ics_209_counties_all,
      list_ics_209_states_all,
      ics_list_complexes_edited_names_all,
      ics_list_fire_and_complex_names_all,
      ics_209_cause_edit_all), ~ gsub("\\s*;\\s*" , ";" ,  .))) 