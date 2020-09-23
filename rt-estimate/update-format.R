# Packages ----------------------------------------------------------------
library(magrittr)
library(data.table, quietly = TRUE)
library(EpiNow2, quietly = TRUE)
library(dplyr)
library(tidyr)
library(purrr)

# Get national -------------------------------------------------------------

estimates <- list("cases_test",
                  "cases_hosp",
                  "deaths_death")
names(estimates) <- estimates

# Get Rt only
summary <- estimates %>%
  purrr::map(~ readr::read_csv(paste0("rt-estimate/estimate/", ., "/summary/rt.csv"))) %>%
  purrr::map_dfr(~ .x, .id = "source") %>%
  # Filter out "estimate based on partial data"
  dplyr::filter(type == "estimate")

# England test cases + deaths were estimated separately
eng_cases <- readr::read_csv("rt-estimate/estimate/cases_test/region/England/2020-08-26/rt_cases_test.csv") %>%
  dplyr::filter(type == "estimate") %>%
  dplyr::mutate(source = "cases_test")

eng_deaths <- readr::read_csv("rt-estimate/estimate/deaths_death/region/England/2020-08-26/rt_deaths_death.csv") %>%
  dplyr::filter(type == "estimate") %>%
  dplyr::mutate(source = "deaths_death")

summary <- summary %>%
  dplyr::filter(!(source == "cases_test" & region == "England" | 
                    source == "deaths_death" & region == "England")) %>%
  dplyr::bind_rows(eng_cases, eng_deaths)
  
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

# Save for use later in plot-data.R
saveRDS(max(estimate_dates$min_date), "utils/earliest_estimate.rds")
saveRDS(min(estimate_dates$max_date), "utils/latest_estimate.rds")

# summary_wide <- summary_wide %>%
#   dplyr::filter(date >= max(estimate_dates$min_date) & date <= min(estimate_dates$max_date))

saveRDS(summary_wide, "rt-estimate/summary_wide.rds")
