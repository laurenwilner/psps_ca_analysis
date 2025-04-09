
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
