# Packages ----------------------------------------------------------------
library(magrittr)
library(data.table, quietly = TRUE)
library(EpiNow2, quietly = TRUE)
library(dplyr)
library(tidyr)
library(purrr)

# Get Rt estimates -------------------------------------------------------------

# Cases positive test
summary_cases <- EpiNow2::get_regional_results(results_dir = "rt-estimate/estimate-six-week/cases_test/region",
                                                      date = "latest")$estimates$summarised

# Admissions - first only
summary_hosp_new <- EpiNow2::get_regional_results(results_dir = "rt-estimate/estimate-six-week/cases_hosp_new/region",
                                               date = "latest")$estimates$summarised

# Deaths
summary_deaths <- EpiNow2::get_regional_results(results_dir = "rt-estimate/estimate-six-week/deaths_death/region",
                                                date = "latest")$estimates$summarised



# Bind and format ---------------------------------------------------------
summary <- dplyr::bind_rows(summary_cases, summary_hosp, summary_deaths, .id = "source") %>%
  dplyr::mutate(source = ifelse(source == 1, "Test-positive cases",
                                ifelse(source == 2, "First hospital admissions",
                                       "Deaths")),
                date_created = Sys.Date()) %>%
  dplyr::filter(variable == "R" & (type == "estimate" | type == "estimate based on partial data")) %>%
  dplyr::select(-strat, -variable, 
                -mean, -sd,
                lower_90 = bottom, upper_90 = top, 
                lower_50 = lower, upper_50 = upper,
                lower_20 = central_lower, upper_20 = central_upper)


# Factor regions ----------------------------------------------------------
source("utils/utils.R")
summary$region = factor(summary$region, 
                        levels = region_names$region_factor)

# Save pure summary -------------------------------------------------------
readr::write_csv(summary, "rt-estimate/estimate-six-week/rt_all_sources.csv")