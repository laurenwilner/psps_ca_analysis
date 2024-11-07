#-------------------------------------------------
# PSPS: Classify events by severity 
# Diagnostic plots to determine thresholds
# November 2024
#-------------------------------------------------
# setup
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(sf, tigris, MetBrewer, lubridate, arrow, raster, tidyverse, scales, knitr, kableExtra, patchwork)

CRS = 'EPSG:3310' # this is california albers

raw_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/raw/psps_circuit_data/")
raw_raster_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/raw/")
clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
plot_dir <- ("~/Desktop/Desktop/epidemiology_PhD/02_projects/psps/plots/")
psps_file_name <- 'ca_zcta_daily_psps_no_washout_2013-2022.csv'

psps_temp <- read.csv(paste0(clean_dir, psps_file_name))

#-------------------------------------------------
# plot distributions
psps <- psps_temp %>% filter(!is.na(pct_cust_out)) %>% mutate(hybrid = total_customers_impacted * pct_cust_out)
psps_plot_dur <- psps %>% group_by(psps_event_id) %>% summarise(duration = mean(duration))

# calculate quantiles
  # quantiles_customers <- quantile(psps_daily$total_customers_impacted, probs = c(.33,.66))
  # quantiles_prop <- quantile(psps_daily$pct_cust_out, probs = c(.33,.66))
  # quantiles_hybrid <- quantile(psps_daily$hybrid, probs = c(.33,.66))
  # quantiles_dur <- quantile(psps_plot_dur$duration, probs = c(.33,.66))
# choose cutoffs by using interpretable values near the tertile values: 
  quantiles_customers <- c(50,500)
  quantiles_hybrid <- c(1, 150)

# left tail of distribution
plot_abs_low <- ggplot(psps %>% filter(total_customers_impacted < 1000), aes(x = total_customers_impacted)) +
  geom_histogram(bins = 30, fill = "#BFDAA4", color = "black") +
  geom_vline(xintercept = quantiles_customers, color = "#446879", linetype = "dashed", size = 1) +
  labs(title = "Left tail: \n total customers impacted (< 1000 customers)",
       x = "total customers impacted",
       y = "count") +
  theme_minimal()
plot_hybrid_low <- ggplot(psps %>% filter(hybrid < 10), aes(x = hybrid)) +
  geom_histogram(bins = 30, fill = "#BFDAA4", color = "black") +
  geom_vline(xintercept = quantiles_hybrid, color = "#446879", linetype = "dashed", size = 1) +
  labs(title = "Left tail: \n hybrid (< 10)",
       x = "hybrid",
       y = "count") +
  xlim(c(NA,10)) +
  theme_minimal()

# right tail of distribution
plot_abs_high <- ggplot(psps %>% filter(total_customers_impacted >5000), aes(x = total_customers_impacted)) +
  geom_histogram(bins = 30, fill = "#BFDAA4", color = "black") +
  geom_vline(xintercept = quantiles_customers, color = "#446879", linetype = "dashed", size = 1) +
  labs(title = "Right tail: \n total customers impacted (>5000 customers)",
       x = "total customers impacted",
       y = "count") +
  xlim(5000, NA) +
  theme_minimal()
plot_hybrid_high <- ggplot(psps %>% filter(hybrid > 150), aes(x = hybrid)) +
  geom_histogram(bins = 30, fill = "#BFDAA4", color = "black") +
  geom_vline(xintercept = quantiles_hybrid, color = "#446879", linetype = "dashed", size = 1) +
  labs(title = "Right tail tail: \n hybrid (> 150)",
       x = "hybrid",
       y = "count") +
  xlim(c(150,NA)) +
  theme_minimal()

# full distribution
plot_abs <- ggplot(psps, aes(x = total_customers_impacted)) +
  geom_histogram(bins = 30, fill = "#BFDAA4", color = "black") +
  geom_vline(xintercept = quantiles_customers, color = "#446879", linetype = "dashed", size = 1) +
  labs(title = "Full distribution: total customers impacted",
       x = "total customers impacted",
       y = "count") +
  theme_minimal()
plot_hybrid <- ggplot(psps, aes(x = hybrid)) +
  geom_histogram(bins = 30, fill = "#BFDAA4", color = "black") +
  geom_vline(xintercept = quantiles_hybrid, color = "#446879", linetype = "dashed", size = 1) +
  labs(title = "Full distribution: hybrid",
       x = "hybrid",
       y = "count") +
  theme_minimal()

((plot_abs_low + plot_abs + plot_abs_high) / (plot_hybrid_low + plot_hybrid + plot_hybrid_high)) + 
    plot_annotation(caption = "Hybrid metric is calculated as the product of the total customers out and the percent of customers out. \n All plots are subset to greater than 0 counts (for total customers impacted and duration). \n Dashed lines represent approximately the 33rd and 66th percentiles of the distribution.")

