# Find peaks and valleys
library(magrittr); library(ggplot2)
source("utils/utils.R")
summary <- readRDS("rt-estimate/summary.rds")
summary_source_region <- split(summary, summary$source)
summary_source_region <- purrr::map(summary_source_region, ~ split(.x, .x$region))


# Peaks
peaks <- purrr::map_depth(summary_source_region, .depth = 2,
                          ~ dplyr::mutate(., date_n = seq_along(date)) %>%
                            dplyr::filter(date_n %in% (quantmod::findPeaks(.x$median,
                                                                           thresh = 0.0005)-1))) 

# Valleys
valleys <- purrr::map_depth(summary_source_region, .depth = 2,
                          ~ dplyr::mutate(., date_n = seq_along(date)) %>%
                            dplyr::filter(date_n %in% (quantmod::findValleys(.x$median, 
                                                                             thresh = 0.0005)-1)))

# See example
summary_source_region$cases_test$`North East and Yorkshire` %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = median)) +
  geom_point(aes(y = median), 
             data = peaks$cases_test$`North East and Yorkshire`,
             colour = "green") +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90), 
                 data = peaks$cases_test$`North East and Yorkshire`) +
  geom_point(aes(y = median), 
             data = valleys$cases_test$`North East and Yorkshire`, 
             colour = "red") +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90), 
                 data = valleys$cases_test$`North East and Yorkshire`)

####
# Peaks
peaks <- purrr::map_depth(summary_source_region, .depth = 2,
                          ~ dplyr::mutate(., date_n = seq_along(date)) %>%
                            dplyr::filter(date_n %in% (quantmod::findPeaks(.x$median,
                                                                           thresh = 0.0005)-1)) %>%
                            dplyr::mutate(diff_date = date_n - dplyr::lag(date_n, 1)) %>%
                            dplyr::filter(is.na(diff_date) | diff_date > 3)) # At least 1 week between peaks

peak_bind <- purrr::map(peaks, ~ dplyr::bind_rows(., .id = "region")) %>%
  dplyr::bind_rows(.id = "source") %>%
  dplyr::mutate(point = "peak")

# Valleys
valleys <- purrr::map_depth(summary_source_region, .depth = 2,
                            ~ dplyr::mutate(., date_n = seq_along(date)) %>%
                              dplyr::filter(date_n %in% (quantmod::findValleys(.x$median, 
                                                                               thresh = 0.0005)-1)) %>%
                              dplyr::mutate(diff_date = date_n - dplyr::lag(date_n, 1)) %>%
                              dplyr::filter(is.na(diff_date) | diff_date > 3))

valley_bind <- purrr::map(valleys, ~ dplyr::bind_rows(., .id = "region")) %>%
  dplyr::bind_rows(.id = "source") %>%
  dplyr::mutate(point = "valley")

# See example
summary %>%
  dplyr::filter(region == "North East and Yorkshire") %>%
  ggplot(aes(x = date, colour = source)) +
  geom_line(aes(y = median)) +
  geom_point(aes(y = median), 
             data = peaks$cases_test$`North East and Yorkshire`, 
             colour = colours["cases_test"]) +
  # geom_linerange(aes(ymin = lower_90, ymax = upper_90), 
  #                data = peaks$cases_test$`North East and Yorkshire`) +
  geom_point(aes(y = median), 
             data = valleys$cases_test$`North East and Yorkshire`, 
             colour = colours["cases_test"]) +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90), 
                 data = valleys$cases_test$`North East and Yorkshire`) 


# Join peaks and valleys to full summary data
peaks_valleys <- summary %>%
  dplyr::left_join(dplyr::select(peak_bind, region, date, source, peak = point), 
                   by = c("region", "date", "source")) %>%
  dplyr::left_join(dplyr::select(valley_bind, region, date, source, valley = point), 
                   by = c("region", "date", "source")) %>%
  dplyr::group_by(region, source) %>%
  dplyr::mutate(date_n = seq_along(date),
                peak_valley_type = ifelse(!is.na(peak), peak,
                                          valley),
                peak_valley_date = ifelse(!is.na(peak_valley_type), date_n, NA),
                peak_start = ifelse(!is.na(peak), date_n, NA)) %>%
  tidyr::fill(peak_start, .direction = "down") %>%
  dplyr::mutate(peak_start = ifelse(is.na(peak_start), 1, peak_start)) %>%
  dplyr::group_by(region, source, peak_start) %>%
  dplyr::mutate(n = dplyr::n(),
                midpt = ifelse(date_n == peak_start + round(n / 2), date_n, NA),
                peak_to_midpt = midpt - peak_start) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(region, source) %>%
  tidyr::fill(midpt, .direction = "down") %>%
  dplyr::mutate(midpt = ifelse(is.na(midpt), 1, midpt))
  
waves <- peaks_valleys %>%
  dplyr::group_by(region, source, midpt) %>%
  dplyr::summarise(
    n_days = dplyr::n(),
    mean_median = mean(median, na.rm=T),
    mean_lo95 = mean_median - 1.96 * (sd(median, na.rm=T) / n_days),
    mean_hi95 = mean_median + 1.96 * (sd(median, na.rm=T) / n_days))

 
# Period: Time between valleys
period_valley <- valley_bind %>%
  dplyr::group_by(region, source) %>%
  dplyr::summarise(n_valleys = dplyr::n(),
                   period_mean = mean(diff_date, na.rm=T),
                   period_lo95 = period_mean - 1.96 * (sd(diff_date, na.rm=T) / n_valleys),
                   period_hi95 = period_mean + 1.96 * (sd(diff_date, na.rm=T) / n_valleys))

period_pre_testing <- valley_bind %>%
  dplyr::filter(date < as.Date("2020-05-03") & 
                  source == "cases_test") %>%
  dplyr::group_by(region, source) %>%
  dplyr::summarise(n_valleys = dplyr::n(),
                   period_mean = mean(diff_date, na.rm=T),
                   period_lo95 = period_mean - 1.96 * (sd(diff_date, na.rm=T) / n_valleys),
                   period_hi95 = period_mean + 1.96 * (sd(diff_date, na.rm=T) / n_valleys))

period_post_testing <- valley_bind %>%
  dplyr::filter(date >= as.Date("2020-05-03") & 
                  source == "cases_test") %>%
  dplyr::group_by(region, source) %>%
  dplyr::summarise(n_valleys = dplyr::n(),
                   period_mean = mean(diff_date, na.rm=T),
                   period_lo95 = period_mean - 1.96 * (sd(diff_date, na.rm=T) / n_valleys),
                   period_hi95 = period_mean + 1.96 * (sd(diff_date, na.rm=T) / n_valleys))

# Time between peaks
period_peak <- peak_bind %>%
  dplyr::group_by(region, source) %>%
  dplyr::summarise(n_peaks = dplyr::n(),
                   period_mean = mean(diff_date, na.rm=T),
                   period_lo95 = period_mean - 1.96 * (sd(diff_date, na.rm=T) / n_peaks),
                   period_hi95 = period_mean + 1.96 * (sd(diff_date, na.rm=T) / n_peaks))



# 
summary %>%
  dplyr::filter(region == "Midlands") %>%
  ggplot(aes(x = date, colour = source)) +
  geom_line(aes(y = median)) +
  geom_point(aes(y = median), 
             data = peaks$deaths_death$`Midlands`, 
             colour = colours["cases_test"])
