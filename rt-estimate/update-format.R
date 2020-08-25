# Packages ----------------------------------------------------------------
library(magrittr)
library(data.table, quietly = TRUE)
library(EpiNow2, quietly = TRUE)
library(dplyr)
library(tidyr)
library(purrr)

# Get national -------------------------------------------------------------

estimates <- list("cases_blend",
                  "cases_hosp",
                  "deaths_blend") # deaths_death deaths_publish cases_publish cases_test
names(estimates) <- estimates

# Get Rt only
summary <- estimates %>%
  purrr::map(~ readr::read_csv(paste0("rt-estimate/estimate/", ., "/summary/rt.csv"))) %>%
  purrr::map_dfr(~ .x, .id = "source") %>%
  # Filter out "estimate based on partial data"
  dplyr::filter(type == "estimate")

# Factor regions for consistent plot alignment
source("utils/utils.R")
summary$region = factor(summary$region, 
                        levels = region_names$region_factor)

saveRDS(summary, "rt-estimate/summary.rds")


# Take ratios -------------------------------------------------------------
summary_wide <- summary %>%
  tidyr::pivot_wider(names_from = source, 
                     values_from = c("median","lower_90", "upper_90", "lower_50", "upper_50"))  %>%
  dplyr::mutate(
    # cases_blend / deaths
    caseb_deathb_med = median_cases_blend / median_deaths_blend,
    caseb_deathb_l90 = lower_90_cases_blend / lower_90_deaths_blend,
    caseb_deathb_u90 = upper_90_cases_blend / upper_90_deaths_blend,
    caseb_deathb_l50 = lower_50_cases_blend / lower_50_deaths_blend,
    caseb_deathb_u50 = upper_50_cases_blend / upper_50_deaths_blend,
    # cases_blend / cases_hosp
    caseb_hosp_med = median_cases_blend / median_cases_hosp,
    caseb_hosp_l90 = lower_90_cases_blend / lower_90_cases_hosp,
    caseb_hosp_u90 = upper_90_cases_blend / upper_90_cases_hosp,
    caseb_hosp_l50 = lower_50_cases_blend / lower_50_cases_hosp,
    caseb_hosp_u50 = upper_50_cases_blend / upper_50_cases_hosp,
    # cases_hosp / deaths_blend
    hosp_deathb_med = median_cases_hosp / median_deaths_blend,
    hosp_deathb_l90 = lower_90_cases_hosp / lower_90_deaths_blend,
    hosp_deathb_u90 = upper_90_cases_hosp / upper_90_deaths_blend,
    hosp_deathb_l50 = lower_50_cases_hosp / lower_50_deaths_blend,
    hosp_deathb_u50 = upper_50_cases_hosp / upper_50_deaths_blend)

# Filter to where all ratios are available (ie max date of deaths as that is longest delay)
estimate_dates <- dplyr::group_by(summary, source, region) %>%
  dplyr::summarise(max_date = max(date),
                   min_date = min(date),
                   .groups = "drop_last")

# Save for use later in plot-data.R
saveRDS(max(estimate_dates$min_date), "utils/earliest_estimate.rds")
saveRDS(min(estimate_dates$max_date), "utils/latest_estimate.rds")

summary_wide <- summary_wide %>%
  dplyr::filter(date >= max(estimate_dates$min_date) & date <= min(estimate_dates$max_date))

if(length(seq.Date(from = min(summary_wide$date), to = max(summary_wide$date), by = 1)) 
   != (length(summary_wide$date) / length(unique(summary_wide$region)))) {
  warning("Not all regions have the same sequence of dates; or missing/duplicate days in sequence")
}

saveRDS(summary_wide, "rt-estimate/summary_wide.rds")
