# Packages ----------------------------------------------------------------
library(magrittr)
library(data.table, quietly = TRUE)
library(EpiNow2, quietly = TRUE)
library(dplyr)
library(tidyr)
library(purrr)

# Get Rt estimates -------------------------------------------------------------

# Cases positive test
summary_cases <- EpiNow2::get_regional_results(results_dir = "rt-estimate/estimate-all-time/cases_test/region",
                                                      date = "latest")$estimates$summarised

summary_cases <- summary_cases %>%
  dplyr::mutate(source = "cases_test")

# Admissions
summary_hosp <- EpiNow2::get_regional_results(results_dir = "rt-estimate/estimate-all-time/cases_hosp/region",
                                               date = "latest")$estimates$summarised
# Deaths
summary_deaths <- EpiNow2::get_regional_results(results_dir = "rt-estimate/estimate-all-time/deaths_death/region",
                                                date = "2020-09-17")$estimates$summarised


# Format
summary <- dplyr::bind_rows(summary_cases, summary_hosp, summary_deaths, .id = "source") %>%
  dplyr::mutate(source = ifelse(source == 1, "cases_test",
                                ifelse(source == 2, "cases_hosp",
                                       "deaths_death"))) %>%
  dplyr::filter(variable == "R" & type == "estimate") %>%
  dplyr::select(-strat, -variable, -mean, -sd,
                source,
                lower_90 = bottom, upper_90 = top,
                lower_50 = lower, upper_50 = upper,
                lower_20 = central_lower, upper_20 = central_upper)


# Factor regions ----------------------------------------------------------
source("utils/utils.R")
summary$region = factor(summary$region, 
                        levels = region_names$region_factor)


# Save pure summary -------------------------------------------------------
saveRDS(summary, "rt-estimate/estimate-all-time/summary.rds")


# Take ratios -------------------------------------------------------------
summary_wide <- summary %>%
  tidyr::pivot_wider(names_from = source, 
                     values_from = c("median", 
                                     "lower_50", "upper_50",
                                     "lower_90", "upper_90"))  %>%
  dplyr::mutate(
    # cases / deaths
    case_death_med = median_cases_test / median_deaths_death,
    case_death_l90 = lower_90_cases_test / lower_90_deaths_death,
    case_death_u90 = upper_90_cases_test / upper_90_deaths_death,
    case_death_l50 = lower_50_cases_test / lower_50_deaths_death,
    case_death_u50 = upper_50_cases_test / upper_50_deaths_death,
    # cases / cases_hosp
    case_hosp_med = median_cases_test / median_cases_hosp,
    case_hosp_l90 = lower_90_cases_test / lower_90_cases_hosp,
    case_hosp_u90 = upper_90_cases_test / upper_90_cases_hosp,
    case_hosp_l50 = lower_50_cases_test / lower_50_cases_hosp,
    case_hosp_u50 = upper_50_cases_test / upper_50_cases_hosp,
    # cases_hosp / deaths_death
    hosp_death_med = median_cases_hosp / median_deaths_death,
    hosp_death_l90 = lower_90_cases_hosp / lower_90_deaths_death,
    hosp_death_u90 = upper_90_cases_hosp / upper_90_deaths_death,
    hosp_death_l50 = lower_50_cases_hosp / lower_50_deaths_death,
    hosp_death_u50 = upper_50_cases_hosp / upper_50_deaths_death)

# Filter to where all ratios are available (ie max date of deaths as that is longest delay)
estimate_dates <- dplyr::group_by(summary, source, region) %>%
  dplyr::summarise(max_date = max(date),
                   min_date = min(date),
                   .groups = "drop_last")

saveRDS(summary_wide, "rt-estimate/estimate-all-time/summary_wide.rds")

# Save for use later in plot-data.R
saveRDS(max(estimate_dates$min_date), "utils/earliest_estimate.rds")
saveRDS(min(estimate_dates$max_date), "utils/latest_estimate.rds")
