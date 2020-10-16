library(magrittr); library(ggplot2)

# Get summary
source("utils/utils.R")

summary <- readRDS("rt-estimate/estimate-all-time/summary_truncated.rds")
summary <- dplyr::mutate(summary,
                         week = lubridate::week(date),
                         month = lubridate::month(date))

summary_source_region <- split(summary, summary$source)
summary_source_region <- purrr::map(summary_source_region, ~ split(.x, .x$region))


# Crossing points ---------------------------------------------------------
# Number of times the daily median Rt estimates crosses 
#  the median of the Rt estimate over the whole time-series
cross_points <- purrr::map_depth(.depth = 2,
                                 summary_source_region, 
                                 ~ tsfeatures::crossing_points(.x[["median"]])) %>%
  dplyr::bind_rows(.id = "region") %>%
  t() %>%
  janitor::row_to_names(1) %>%
  tibble::as_tibble(rownames = "region") %>%
  dplyr::mutate(dplyr::across(2:4, as.numeric)) %>%
  tidyr::pivot_longer(-region, names_to = "source", values_to = "crosses")

cross_by_source <- split(cross_points, cross_points$source)

cross_all_oth_regions <- cross_by_source %>%
  purrr::map(., ~ dplyr::filter(., !region %in% c("North East and Yorkshire", 
                                                  "Midlands", "England"))) %>%
  purrr::map(., ~ t.test(.$crosses)) %>%
  purrr::map(., ~ purrr::keep(., names(.) %in% c("estimate", "conf.int")))


# Identify peaks/troughs -----------------------------------------------------------------
summary_peaks_valleys <- summary %>%
  dplyr::group_by(region, source) %>%
  dplyr::mutate(diff = median - dplyr::lag(median, 1),
                peak = ifelse(diff <= 0 & 
                                dplyr::lag(diff) > 0, # Rising yesterday, flat or decreasing today
                              as.character(dplyr::lag(date, 1)), NA), 
                peak_adj = ifelse(!is.na(dplyr::lead(peak, 1)) |
                                    !is.na(dplyr::lead(peak, 2)) |
                                    !is.na(dplyr::lead(peak, 3)) |
                                    !is.na(dplyr::lead(peak, 4)) |
                                    !is.na(dplyr::lead(peak, 5)) |
                                    !is.na(dplyr::lead(peak, 6)) | # Remove multiple peaks within a week
                                    median < median(median),  # True peak
                                  NA, peak),  
                valley = ifelse(diff >= 0 & 
                                  dplyr::lag(diff) < 0, 
                              as.character(dplyr::lag(date, 1)), NA),
                valley_adj = ifelse(!is.na(dplyr::lead(valley, 1)) |
                                    !is.na(dplyr::lead(valley, 2)) |
                                    !is.na(dplyr::lead(valley, 3)) |
                                    !is.na(dplyr::lead(valley, 4)) |
                                    !is.na(dplyr::lead(valley, 5)) |
                                    !is.na(dplyr::lead(valley, 6)) |
                                    !is.na(dplyr::lead(valley, 7)) |
                                    !is.na(dplyr::lag(peak, 1)) |
                                    median > median(median), 
                                  NA, valley))

# Number of peaks/valleys by region
region_peaks_valleys <- dplyr::filter(summary_peaks_valleys, 
                                      !is.na(peak_adj) |
                                      !is.na(valley_adj)) %>%
  dplyr::summarise(n_peaks = sum(!is.na(peak_adj)),
                   n_valleys = sum(!is.na(valley_adj)))

time_between_valleys <- dplyr::filter(summary_peaks_valleys, 
                                     !is.na(valley_adj)) %>%
  dplyr::mutate(time_between_valleys = as.Date(valley_adj) - dplyr::lag(as.Date(valley_adj), 1))

time_between_peaks <- dplyr::filter(summary_peaks_valleys, 
                                      !is.na(peak))%>%
  dplyr::mutate(time_between_peaks = as.Date(peak) - dplyr::lag(as.Date(peak), 1))


peaks_troughs <- list(
  "summary_peaks_valleys" = summary_peaks_valleys,
  "region_peaks_valleys" = region_peaks_valleys,
  "time_between_valleys" = time_between_valleys,
  "time_between_peaks" = time_between_peaks)

saveRDS(peaks_troughs, "compare/rt-comparison/peaks_troughs.rds")


# Duration of oscillations by region --------------------------------------
# NE & Yorkshire
ney <- peaks_troughs$time_between_valleys %>%
  dplyr::filter(region == "North East and Yorkshire") 
ney <- split(ney, ney$source) %>%
  purrr::map(., ~ t.test(.$time_between_valleys)) %>%
  purrr::map(., ~ purrr::keep(., names(.) %in% c("estimate", "conf.int")))

# Midlands
mids <- peaks_troughs$time_between_valleys %>%
  dplyr::filter(region == "Midlands")
mids <- split(mids, mids$source) %>%
  purrr::map(., ~ t.test(.$time_between_valleys)) %>%
  purrr::map(., ~ purrr::keep(., names(.) %in% c("estimate", "conf.int")))

# All other regions
alloth <- peaks_troughs$time_between_valleys %>%
  dplyr::filter(!region %in% c("North East and Yorkshire", "Midlands", "England")) 
alloth <- split(alloth, alloth$source) %>%
  purrr::map(., ~ t.test(.$time_between_valleys)) %>%
  purrr::map(., ~ purrr::keep(., names(.) %in% c("estimate", "conf.int")))


# Tabulate ----------------------------------------------------------------

summary_valley <- time_between_valleys %>%
  dplyr::mutate(days_since_valley = as.numeric(as.Date(peak_adj) - dplyr::lag(as.Date(peak_adj), 1)),
                median_90 = stringr::str_c(round(median, 2), 
                                           " (", round(lower_90, 2), " - ", round(upper_90, 2), ")")) %>%
  dplyr::select(region, source, date, median_90, days_since_valley)



summary_peak <- time_between_peaks %>%
  group_by(region, source) %>%
  dplyr::mutate(days_since_peak = as.numeric(as.Date(peak_adj) - dplyr::lag(as.Date(peak_adj), 1)),
                median_90 = stringr::str_c(round(median, 2), 
                                           " (", round(lower_90, 2), " - ", round(upper_90, 2), ")")) %>%
  dplyr::select(region, source, date, median_90, days_since_peak)

summary_waves <- bind_rows(summary_valley, summary_peak)


