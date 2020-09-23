# Get estimates
summary_wide <- readRDS("rt-estimate/summary_wide.rds")
source("utils/utils.R")



# STL decomposition -------------------------------------------------------

eng <- data[data$region == "Midlands",]
eng <- merge(eng, summary_wide, by = c("region", "date"))


eng_ts <- ts(eng$median_cases_hosp, start = min(eng$date), frequency = 7)

eng_ts <- eng_ts[!is.na(eng_ts)]

eng_ts %>%
  forecast::mstl(robust=TRUE) %>%
  forecast::autoplot()

sts <- StructTS(eng_ts)
sts$coef["slope"]

plot(eng_ts) + 
  abline(min(eng_ts, na.rm=T), StructTS(eng_ts[54:length(eng_ts)])$coef["slope"])

eng_stl <- forecast::mstl(eng_ts)

eng_ts %>%
  decompose(type = "multiplicative") %>%
  forecast::autoplot()

fit <- eng_ts %>%
  seasonal::seas(x11="")

fit <- eng_ts %>% forecast::mstl()

tc <- forecast::trendcycle(eng_stl)


# Find peaks --------------------------------------------------------------
# Example:
# data <- summary_wide[summary_wide$region == "Scotland",]
# variable <- "median_cases_test"
# window <- 7

wave_features <- function(data, variable, window){
  
  # Clean
  region <- data$region
  colnames(data) <- gsub(variable, "estimate", x = colnames(data))
  data <- data[which.min(is.na(data$estimate)):length(data$date),c("date", "estimate")]
  newrowmin <- data.frame(date = min(data$date)-1, estimate = NA) # Add empty row in case maximum at start
  newrowmax <- data.frame(date = max(data$date)+1, estimate = NA)
  data <- rbind(data, newrowmin, newrowmax) 
  data_zoo <- zoo::zoo(data["estimate"], order.by = data$date)
  
  # Find peaks (using 5 day rolling window)
  peaks <- zoo::rollapply(data_zoo, window, function(x) which.max(x)==2)
  # Find troughs (using 5 day rolling window)
  raw_troughs <- zoo::rollapply(data_zoo, window, function(x) which.min(x)==2)
  # Get rid of tiny variation (peak and trough 1 day apart)
  wave <- zoo::cbind.zoo(peaks, raw_troughs)
  colnames(wave) <- c("peaks", "raw_troughs")
  wave$keep_trough <- ifelse(stats::lag(wave$peaks, n = -1) == TRUE & 
                               wave$raw_troughs == TRUE, "drop", "keep")
  wave$trough <- ifelse(wave$keep == "keep" & wave$raw_troughs == TRUE, TRUE, FALSE)
  
  peaks <- zoo::index(wave$peaks)[wave$peaks == TRUE] -2 # Calibrate (overshoots by 2d) 
  troughs <- zoo::index(wave$trough)[wave$trough == TRUE] -2
  
  peaks_value <- as.data.frame(data[data$date %in% peaks, c("date", "estimate")])
  troughs_value <- as.data.frame(data[data$date %in% troughs, c("date", "estimate")])
  
  values <- merge(peaks_value, troughs_value, all = TRUE, by = "date")
  colnames(values) <- c("date", "peak", "trough")
  
  if((nrow(values) - length(zoo::na.locf(values$trough))) > 0 ) {
    values$trough_match <- c(rep(NA, times = (nrow(values) - length(zoo::na.locf(values$trough)))), 
                             zoo::na.locf(values$trough))
  } else {
    values$trough_match <- zoo::na.locf(values$trough)
  }
  
  values$centre <- rowMeans(values[,c("peak", "trough_match")])
  values$amplitude <- values$peak - values$centre
  
  
  # Wave characteristics
  wave_features_list <- list(
    "peaks" = peaks,
    "peak_values" = peaks_value,
    "n_peaks" = length(peaks),
    "troughs" = troughs,
    "trough_values" = troughs_value,
    "n_troughs" = length(troughs),
    "centres" = c(NA, values$centre[!is.na(values$amplitude)]),
    "amplitudes" = c(NA, values$amplitude[!is.na(values$amplitude)]),
    "periods" = as.numeric(diff.Date(peaks)),
    "plot_peak_trough" = data %>% 
                          ggplot(aes(x = date)) + 
                          geom_line(aes(y = estimate)) + 
                          geom_vline(xintercept = peaks, lty = 2) +
                          geom_vline(xintercept = troughs, lty = 3) + 
                          labs(x = region) + 
                          theme_classic() )
  wave_features_list
}

summary_group <- split(summary_wide, summary_wide$region)

all_wave <- list(
  cases = purrr::map(summary_group, ~ wave_features(.x, "median_cases_test", 7)),
  hosp = purrr::map(summary_group, ~ wave_features(.x, "median_cases_hosp", 7)),
  deaths = purrr::map(summary_group, ~ wave_features(.x, "median_deaths_death", 7)))



# Regional features -------------------------------------------------------
regional_wave_features <- all_wave %>%
  purrr::transpose() %>%
  purrr::discard(.p = names(.) %in% c("England", "Scotland", "Wales", "Northern Ireland"))

# Average amplitude among all regions of cases, hosp, deaths
amplitudes <- regional_wave_features %>%
  purrr::map_depth(.depth = 2, ~ .$amplitudes) %>%
  purrr::map_depth(.depth = 2, ~ c(., rep(NA, length.out = 7-length(.)))) %>%
  dplyr::bind_rows(.id = "region")
c(t.test(amplitudes$cases)$estimate, 
   t.test(amplitudes$cases)$conf.int)
c(t.test(amplitudes$hosp)$estimate,
  t.test(amplitudes$hosp)$conf.int)
c(t.test(amplitudes$deaths)$estimate,
  t.test(amplitudes$deaths)$conf.int)

# Average period among all regions of cases, hosp, deaths
periods <- regional_wave_features %>%
  purrr::map_depth(.depth = 2, ~ .$periods) %>%
  purrr::map_depth(.depth = 2, ~ c(., rep(NA, length.out = 6-length(.)))) %>%
  dplyr::bind_rows(.id = "region")
cases_t <- t.test(periods$cases)
hosp_t <- t.test(periods$hosp)
deaths_t <- t.test(periods$deaths)





# Averages by region
amplitudes <- regional_wave_features %>%
  purrr::map_depth(.depth = 2, ~ t.test(.$amplitudes, na.rm=T)) %>%
  purrr::transpose()

amplitudes_mean <- amplitudes %>%
  purrr::map_depth(.depth = 2, ~ purrr::keep(., names(.) %in% c("estimate"))) %>%
  purrr::map( ~ dplyr::bind_rows(., .id = "region")) %>%
  dplyr::bind_rows(., .id = "source")

amplitudes_ci <- amplitudes %>%
  purrr::map_depth(.depth = 2, ~ purrr::keep(., names(.) %in% c("conf.int"))) %>%
  purrr::map_depth(.depth = 2, ~ dplyr::bind_rows(.)) %>%
  purrr::map( ~ dplyr::bind_rows(., .id = "region")) %>%
  dplyr::bind_rows(., .id = "source") %>%
  dplyr::mutate(conftype = rep(c("lower", "upper"), times = nrow(amplitudes_mean))) %>%
  tidyr::pivot_wider(names_from = conftype, values_from = conf.int) %>%
  dplyr::left_join(amplitudes_mean, by = c("source", "region")) %>%
  dplyr::mutate(lower = ifelse(lower < 0 , 0, lower)) %>%
  tidyr::pivot_wider(names_from = source, values_from = c(estimate, lower, upper))



# Average period of cases, hosp, deaths
period <- regional_wave_features %>%
  purrr::map_depth(.depth = 2, ~ t.test(.$periods, na.rm=T)) %>%
  purrr::transpose()

period_mean <- period %>%
  purrr::map_depth(.depth = 2, ~ purrr::keep(., names(.) %in% c("estimate"))) %>%
  purrr::map( ~ dplyr::bind_rows(., .id = "region")) %>%
  dplyr::bind_rows(., .id = "source")

period_ci <- period %>%
  purrr::map_depth(.depth = 2, ~ purrr::keep(., names(.) %in% c("conf.int"))) %>%
  purrr::map_depth(.depth = 2, ~ dplyr::bind_rows(.)) %>%
  purrr::map( ~ dplyr::bind_rows(., .id = "region")) %>%
  dplyr::bind_rows(., .id = "source") %>%
  dplyr::mutate(conftype = rep(c("lower", "upper"), times = nrow(period_mean))) %>%
  tidyr::pivot_wider(names_from = conftype, values_from = conf.int) %>%
  dplyr::left_join(period_mean, by = c("source", "region")) %>%
  dplyr::mutate(lower = ifelse(lower < 0 , 0, lower)) %>%
  tidyr::pivot_wider(names_from = source, values_from = c(estimate, lower, upper))



# Number of peaks
n_peaks <- regional_wave_features %>%
  purrr::map_depth(.depth = 2, ~ .$n_peaks) %>%
  dplyr::bind_rows(.id = "region")




mean_over_series <- function(data, variable){
  
  colnames(data) <- gsub(variable, "rt_estimate", x = colnames(data))
  
  median_split <- split(data, data$region)
  
  rt_test <- median_split %>%
    purrr::map( ~ t.test(.x[,"rt_estimate"], na.rm=T))
  
  rt_mean <- rt_test %>%
    purrr::map( ~ purrr::keep(., names(.) %in% c("estimate"))) %>%
    purrr::map( ~ dplyr::bind_rows(.)) %>%
    dplyr::bind_rows(., .id = "id")
  
  mean_ci <- rt_test %>%
    purrr::map( ~ purrr::keep(., names(.) %in% c("conf.int"))) %>%
    purrr::map( ~ dplyr::bind_rows(.)) %>%
    dplyr::bind_rows(., .id = "id") %>%
    dplyr::mutate(conftype = rep(c("lower", "upper"), times = length(unique(data$region)))) %>%
    tidyr::pivot_wider(names_from = conftype, values_from = conf.int) %>%
    dplyr::left_join(rt_mean, by = "id")
  
  colnames(mean_ci) <- c("region", 
                         paste0(variable, "_lower"), paste0(variable, "_upper"), paste0(variable, "_mean")) 
  
  mean_ci
}

cases_median_mean <- mean_over_series(median, "median_cases_test")
hosp_median_mean <- mean_over_series(median, "median_cases_hosp")
deaths_median_mean <- mean_over_series(median, "median_deaths_death")

median_mean <- dplyr::left_join(cases_median_mean, hosp_median_mean, by = "region") %>%
  dplyr::left_join(deaths_median_mean, by = "region") %>%
  dplyr::filter(region %in% region_names$value$nhsregions)


median_region <- dplyr::filter(median, region %in% region_names$value$nhsregions)
c(t.test(median_region$median_cases_test)$estimate, 
  t.test(median_region$median_cases_test)$conf.int)
c(t.test(median_region$median_cases_hosp)$estimate,
  t.test(median_region$median_cases_hosp)$conf.int)
c(t.test(median_region$median_deaths_death)$estimate,
  t.test(median_region$median_deaths_death)$conf.int)

lower90_region <- dplyr::filter(lower90, region %in% region_names$value$nhsregions)
c(t.test(lower90_region$lower_90_cases_test)$estimate, 
  t.test(lower90_region$lower_90_cases_test)$conf.int)
c(t.test(lower90_region$lower_90_cases_hosp)$estimate,
  t.test(lower90_region$lower_90_cases_hosp)$conf.int)
c(t.test(lower90_region$lower_90_deaths_death)$estimate,
  t.test(lower90_region$lower_90_deaths_death)$conf.int)

upper90_region <- dplyr::filter(upper90, region %in% region_names$value$nhsregions)
c(t.test(upper90_region$upper_90_cases_test)$estimate, 
  t.test(upper90_region$upper_90_cases_test)$conf.int)
c(t.test(upper90_region$upper_90_cases_hosp)$estimate,
  t.test(upper90_region$upper_90_cases_hosp)$conf.int)
c(t.test(upper90_region$upper_90_deaths_death)$estimate,
  t.test(upper90_region$upper_90_deaths_death)$conf.int)




# Admissions & deaths ------------------------------------

deaths_admissions <- list("deaths" = list(), "hosp" = list())
for(i in region_names$value$nhsregions) {
  deaths_admissions$deaths[[i]] <- regional_wave_features[[i]][["deaths"]][["peak_values"]]
  deaths_admissions$hosp[[i]] <- regional_wave_features[[i]][["hosp"]][["peak_values"]]
}

spring_peaks <- deaths_admissions %>%
  purrr::transpose() %>%
  purrr::map_depth(.depth = 2, ~ dplyr::filter(., date <= as.Date("2020-07-13"))) %>%
  purrr::map_depth(.depth = 1, ~ dplyr::bind_rows(., .id = "source")) %>%
  dplyr::bind_rows(., .id = "region") %>%
  dplyr::mutate(region = factor(region, region_names$value$nhsregions))

# # Mean and CIs around ratio of case to admissions and deaths Rts
# t.test(april$caseb_hosp_med)
# t.test(april$caseb_deathb_med)

# Ratios
# april_admissions <- tibble::tibble(date = as.Date(c("2020-04-21",
#                                                      "2020-04-26",
#                                                      "2020-04-24",
#                                                      "2020-04-18",
#                                                      "2020-04-20",
#                                                      "2020-05-12",
#                                                      "2020-05-09")),
#                                    region = region_names$value$nhsregions,
#                                    source = rep("hosp", 7)) %>%
#   left_join(summary_ratios, by = c("region", "date")) %>%
#   select(date, region, source, caseb_hosp_med) %>%
#   left_join(summary %>% filter(`Data source` == "Hospital admissions"), by = c("date", "region"))
# 
# #
# april_deaths <- tibble::tibble(date = as.Date(c("2020-04-24",
#                                                    "2020-04-23",
#                                                    "2020-04-23",
#                                                    "2020-04-17",
#                                                    "2020-04-23",
#                                                    "2020-05-06",
#                                                    "2020-05-11")),
#                                region = region_names$value$nhsregions,
#                                source = rep("deaths", 7)) %>%
#   left_join(summary_ratios, by = c("region", "date")) %>%
#   select(date, region, source, caseb_deathb_med)%>%
#   left_join(summary %>% filter(`Data source` == "Deaths"), by = c("date", "region"))
# 
# april <- dplyr::bind_rows(april_deaths, april_admissions)

# plot_april <- april %>%
#   ggplot(aes(x = region, colour = `Data source`)) +
#   geom_pointrange(aes(y = median, ymin = lower_90, ymax = upper_90)) +
#   scale_color_manual(values = colours) +
#   coord_flip() +
#   labs(x = NULL, y = NULL) +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         legend.margin = margin(0,0,0,0),
#         plot.margin = margin(0,0,0,0),
#         text = element_text(size = 15))
# 
# ggsave("figures/april_admissions_deaths.png")
       

# Mean and CIs around ratio of case to admissions and deaths Rts
t.test(april$caseb_hosp_med)
t.test(april$caseb_deathb_med)
