#-------------------------------------------------
# isee poster 
# author: lauren blair wilner
# august 2024


#-------------------------------------------------
# setup
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(sf, tigris, MetBrewer, lubridate, arrow, raster, tidyverse, scales, knitr, kableExtra, tigris)

CRS = 'EPSG:3310' # this is california albers

raw_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/raw/psps_circuit_data/")
raw_raster_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/raw/")
clean_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
plot_dir <- ("~/Desktop/Desktop/epidemiology_PhD/02_projects/psps/plots/")

colors <- c("#32006e", "#b7a57a")

#-------------------------------------------------
# load data
psps_clean <- read.csv(paste0(clean_dir, "psps_underlying_zcta_clean.csv"))
zcta_ca <- read_parquet(paste0(clean_dir, "us_ca_zcta_shp.parquet"))
zcta_ca <- st_as_sf(zcta_ca, crs = CRS)
counties_ca <- county_subdivisions(06, year = 2019) %>%
                select(c("geometry"))

results_df <- data.frame(type = c("Absolute", "Hybrid"), 
                        or = c(1.18, 0.98), 
                        lower = c(0.99, 0.53), 
                        upper = c(1.40, 1.81))
results_df$type <- factor(results_df$type, levels = rev(c("Absolute", "Hybrid")))
results_older_df <- data.frame(
  age = c("50-64 years", "50-64 years",
          "65-79 years", "65-79 years",
          "80+ years", "80+ years"),
  type = c("Absolute", "Hybrid",
           "Absolute", "Hybrid",
           "Absolute", "Hybrid"),
  or = c(1.205, 0.883,
         1.060, 1.055,
         1.218, 1.598),
  lower = c(0.687, 0.387,
            0.622, 0.499,
            0.680, 0.728),
  upper = c(2.111, 2.013,
            1.808, 2.230,
            2.182, 3.508)
)
results_older_df$type <- factor(results_older_df$type, levels = rev(c("Absolute", "Hybrid")))
results_older_df$age <- factor(results_older_df$age, levels = c("50-64 years", "65-79 years", "80+ years"))

#-------------------------------------------------
# exposure plots 
events_abs <- psps_clean %>%
    filter(year(outage_start) == 2019) %>%
    arrange(desc(total_customers_impacted)) %>% 
    mutate(hybrid = total_customers_impacted * pct_cust_out,
        type = 'absolute space',
        zcta = as.character(zcta)) %>% 
    head(10) %>% 
    left_join(zcta_ca, by = "zcta") %>% 
    st_as_sf()
events_hyb <- psps_clean %>% 
    filter(year(outage_start) == 2019) %>%
    mutate(hybrid = total_customers_impacted * pct_cust_out,
            type = 'hybrid',
        zcta = as.character(zcta)) %>%
    arrange(desc(hybrid)) %>%
    head(10) %>% 
    left_join(zcta_ca, by = "zcta") %>% 
    st_as_sf()

map_abs <- ggplot() + 
    geom_sf(data = counties_ca, fill = "white", color = "gray") + 
    geom_sf(data = events_abs, aes(fill = hybrid), color = "black") + 
    scale_fill_gradient(low = "#e0d7f5", high = "#32006e") + 
    labs(title = "Absolute", 
         fill = "Customers impacted") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_void()

map_hyb <- ggplot() + 
    geom_sf(data = counties_ca, fill = "white", color = "gray") + 
    geom_sf(data = events_hyb, aes(fill = hybrid), color = "black") + 
    scale_fill_gradient(low = "#e0d7f5", high = "#32006e") + 
    labs(title = "Hybrid", 
         fill = "Customers impacted") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_void()

duration_hist <- ggplot(psps_clean %>% filter(duration < 250), aes(x = duration)) + 
    geom_histogram(bins = 100, fill = colors[1]) + 
    labs(title = "Histogram of Outage Duration (hours)", 
        x = "Duration (hours)", 
        y = "Frequency") + 
    theme_minimal()


#-------------------------------------------------
# outcome table: cases by age/race 

# outcome plots:
    # odds ratios adults
    # odds ratios older adults
adults_or_plot <- ggplot(results_df, aes(x = or, y = type, color = type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0) +  # Removed the height to eliminate vertical lines
  geom_vline(xintercept = 1.0, color = "black") +  # Add vertical line at 1.0
  scale_color_manual(values = colors) +
  labs(
    title = "PSPS events and cardiopulmonary ED visits in adults",
    x = "Odds Ratio",
    y = "",
    color = "Top PSPS Type"
  ) +
  scale_x_log10() +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 14, hjust = .5),
  )

older_adults_or_plot <- ggplot(results_older_df, aes(x = or, y = age, color = type)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 1.0, color = "black") +
  scale_x_log10() +
  scale_color_manual(values = colors) +
  labs(
    title = "PSPS events and cardiopulmonary ED visits in older adults",
    x = "Odds Ratio",
    y = "",
    color = "Top PSPS Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = .5),
  )

#-------------------------------------------------
# save plots
ggsave(paste0(plot_dir, "adults_or_plot.png"), adults_or_plot, width = 8, height = 4)
ggsave(paste0(plot_dir, "older_adults_or_plot.png"), older_adults_or_plot, width = 8, height = 4)
ggsave(paste0(plot_dir, "map_abs.png"), map_abs, width = 8, height = 4)
ggsave(paste0(plot_dir, "map_hyb.png"), map_hyb, width = 8, height = 4)
ggsave(paste0(plot_dir, "duration.png"), duration_hist, width = 8, height = 4)




#### Mediation method
We use mediation analysis methods to estimate how much SES disparities in asthma rates in Oklahoma would be reduced had well count at census tract level been fixed to 10.

```{r}

mediation <- cmest(data = mediation_df, model = "rb", 
                          outcome = "ASTHMA", 
                          exposure = "SES_bin", mediator = "WELL", EMint = TRUE,
                          basec = c("female", "age", "white","density"), 
                          mreg = list("poisson"), yreg = "poisson", 
                          a = 1, astar = 0, mval = list(10),
                          estimation = "imputation", inference = "bootstrap",full=FALSE)

summary(mediation)

# the controlled direct effect of SES on asthma rates with well count fixed to M=10 is the CDE: 1.38 (95% CI: 1.32, 1.44)

# portion eliminated by the intervention

PE<-(1.63-1.38)/1.63 ## (TE - CDE)/TE
PE
#15%

```

#### Direct and indirect effect
We use mediation analysis methods to estimate the natural direct and natural indirect effects of SES on asthma rates, as well as the proportion mediated

```{r}

summary(mediation)

# the pure natural direct effect of SES on asthma rates is the PNDE: 1.48 (95% CI: 1.47, 1.50)

# the pure natural indirect effect of SES on asthma rates is the PNIE: 1.08 (95% CI: 1.07, 1.09)

# proportion mediated

PM<-(1.08*(1.10-1))/(1.63-1) ## (PNDE∗(TNIE−1))/(TE−1)
PM
#0.17

### see https://bs1125.github.io/CMAverse/articles/quickstart.html for a guide to the CMAverse package ###
```

