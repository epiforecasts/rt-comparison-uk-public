# Get estimates and data
summary_wide <- readRDS("rt-estimate/summary_wide.rds")



# End of first wave -----------------------------------------------------------------

# Check timing of first wave cross below 1
rt1_cases <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(median_cases_blend < 1 & date > "2020-02-29" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, cases = date) %>%
  dplyr::filter(region %in% region_names$nhsregions)

rt1_hosp <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(median_cases_hosp < 1 & date > "2020-02-29" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, hosp = date)%>%
  dplyr::filter(region %in% region_names$nhsregions)

rt1_deaths <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(median_deaths_blend < 1 & date > "2020-02-29" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, deaths = date)

rt1 <- left_join(rt1_cases, rt1_hosp, by = "region") %>%
  left_join(rt1_deaths, by = "region")%>%
  dplyr::filter(region %in% region_names$nhsregions)


# Find peaks --------------------------------------------------------------

wave_features <- function(data, variable, window){
  
  colnames(data) <- gsub(variable, "estimate", x = colnames(data))
  
  data_zoo <- zoo::zoo(data["estimate"], order.by = data$date)
  data_zoo <- zoo::na.trim(data_zoo, sides = "both")
  
  # Find peaks (using 5 day rolling window)
  peaks <- zoo::rollapply(data_zoo, window, function(x) which.max(x)==2)
  # Find troughs (using 5 day rolling window)
  raw_troughs <- zoo::rollapply(data_zoo, window, function(x) which.min(x)==2)
  # Get rid of tiny variation (peak and trough 1 day apart)
  wave <- zoo::cbind.zoo(peaks, raw_troughs)
  colnames(wave) <- c("peaks", "raw_troughs")
  wave$keep_trough <- ifelse(lag(wave$peaks, -1) == TRUE & 
                               wave$raw_troughs == TRUE, "drop", "keep")
  wave$trough <- ifelse(wave$keep == "keep" & wave$raw_troughs == TRUE, TRUE, FALSE)
  
  peaks <- zoo::index(wave$peaks)[wave$peaks == TRUE] -2
  troughs <- zoo::index(wave$trough)[wave$trough == TRUE] -2
  
  # Wave characteristics
  wave_features_list <- list("peak_trough" = list("peaks" = peaks,
                                             "n_peaks" = length(peaks),
                                             "troughs" = troughs,
                                             "n_troughs" = length(troughs),
                        "period" = list("peak_periods" = diff.Date(peaks),
                                        "peak_period_mean" = mean(diff.Date(peaks)),
                                        "trough_periods" = diff.Date(troughs),
                                        "trough_period_mean" = mean(diff.Date(troughs))),
                        "plot_peak_trough" = data %>% 
                          ggplot(aes(x = date)) + 
                          geom_line(aes(y = estimate)) + 
                          geom_vline(xintercept = peaks, lty = 2) +
                          geom_vline(xintercept = troughs, lty = 3) + 
                          labs(x = data$region) + 
                          theme_classic()))
  wave_features_list
}

summary_group <- split(summary_wide, summary_wide$region)


