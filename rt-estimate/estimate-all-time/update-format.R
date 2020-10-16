# Packages ----------------------------------------------------------------
library(magrittr)
library(data.table, quietly = TRUE)
library(EpiNow2, quietly = TRUE)
library(dplyr)
library(tidyr)
library(purrr)
source("utils/utils.R")

# Truncation dates
trunc_cases <- "2020-08-28"
trunc_deaths <- "2020-08-20"

# Get Rt estimates -------------------------------------------------------------

# Cases positive test
summary_cases <- EpiNow2::get_regional_results(results_dir = "rt-estimate/estimate-all-time/cases_test/region",
                                                      date = "2020-09-16")$estimates$summarised

# Admissions. 2 regional estimates failed and were re-run separately
summary_hosp_0916 <- EpiNow2::get_regional_results(results_dir = "rt-estimate/estimate-all-time/cases_hosp/region",
                                               date = "2020-09-16")$estimates$summarised

summary_hosp_0917 <- EpiNow2::get_regional_results(results_dir = "rt-estimate/estimate-all-time/cases_hosp/region",
                                                   date = "2020-09-17")$estimates$summarised
summary_hosp <- summary_hosp_0916 %>%
  filter(!region %in% c("England", "East of England")) %>%
  bind_rows(summary_hosp_0917)

# Deaths. 1 region failed
summary_deaths_0916 <- EpiNow2::get_regional_results(results_dir = "rt-estimate/estimate-all-time/deaths_death/region",
                                                date = "2020-09-16")$estimates$summarised
summary_deaths_0917 <- EpiNow2::get_regional_results(results_dir = "rt-estimate/estimate-all-time/deaths_death/region",
                                                date = "2020-09-17")$estimates$summarised
summary_deaths <- summary_deaths_0917 %>%
  filter(!region %in% c("Midlands")) %>%
  bind_rows(summary_deaths_0916)


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
summary$region = factor(summary$region, 
                        levels = region_names$region_factor)


# Save pure summary -------------------------------------------------------
saveRDS(summary, "rt-estimate/estimate-all-time/summary-full.rds")


# Truncate Rt estimates ---------------------------------------------------
# Truncate by 5 days
# Cases and admissions: max date is 5th Sept, so 31 August
# Deaths: max date 27 August, so 22 August

summary_trunc <- summary %>%
  dplyr::filter((source %in% c("cases_test", "cases_hosp") & date < trunc_cases) |
                  (source == "deaths_death" & date < trunc_deaths))

saveRDS(summary_trunc, "rt-estimate/estimate-all-time/summary_truncated.rds")

# Get samples -------------------------------------------------------------
regions <- as.list(region_names[["region_factor"]])
names(regions) <- region_names[["region_factor"]]

sources_regions <- list("cases_test" = regions,
                        "cases_hosp" = regions,
                        "deaths_death" = regions)

samples_cases_test <- purrr::map(regions,
                                 ~ readRDS(paste0(
                                   "rt-estimate/estimate-all-time/",
                                   "cases_test", 
                                   "/region/", .x, 
                                   "/2020-09-16/estimate_samples.rds"))) %>%
  purrr::map(., 
             ~ dplyr::filter(., parameter == "R" & type == "estimate")) %>%
  dplyr::bind_rows(.id = "region") %>%
  dplyr::mutate(source = "cases_test")

# Admissions
samples_cases_hosp_16 <- regions %>%
  purrr::keep(., !grepl("England|East of England", names(.))) %>%
  purrr::map(~ readRDS(paste0(
    "rt-estimate/estimate-all-time/",
    "cases_hosp", "/region/", .x, 
    "/2020-09-16/estimate_samples.rds"))) %>%
  purrr::map(., 
             ~ dplyr::filter(., parameter == "R" & type == "estimate")) %>%
  dplyr::bind_rows(.id = "region") 

samples_cases_hosp_17 <- regions %>%
  purrr::keep(., grepl("England|East of England", names(.))) %>%
  purrr::map(~ readRDS(paste0(
    "rt-estimate/estimate-all-time/",
    "cases_hosp", "/region/", .x, 
    "/2020-09-17/estimate_samples.rds"))) %>%
  purrr::map(., 
             ~ dplyr::filter(., parameter == "R" & type == "estimate")) %>%
  dplyr::bind_rows(.id = "region") 

samples_cases_hosp <- bind_rows(samples_cases_hosp_16, samples_cases_hosp_17) %>%
  dplyr::mutate(source = "cases_hosp")

rm(samples_cases_hosp_16, samples_cases_hosp_17)

# Deaths
samples_deaths_16 <- regions %>%
  purrr::keep(., grepl("Midlands", names(.))) %>%
  purrr::map(~ readRDS(paste0(
    "rt-estimate/estimate-all-time/",
    "deaths_death", "/region/", .x, 
    "/2020-09-16/estimate_samples.rds"))) %>%
  purrr::map(., 
             ~ dplyr::filter(., parameter == "R" & type == "estimate")) %>%
  dplyr::bind_rows(.id = "region") 

samples_deaths_17 <- regions %>%
  purrr::keep(., !grepl("Midlands", names(.))) %>%
  purrr::map(~ readRDS(paste0(
    "rt-estimate/estimate-all-time/",
    "deaths_death", "/region/", .x, 
    "/2020-09-17/estimate_samples.rds"))) %>%
  purrr::map(., 
             ~ dplyr::filter(., parameter == "R" & type == "estimate")) %>%
  dplyr::bind_rows(.id = "region") 

samples_deaths <- bind_rows(samples_deaths_16, samples_deaths_17) %>%
  dplyr::mutate(source = "deaths_death")

rm(samples_deaths_16, samples_deaths_17)


# Join all samples
samples <- dplyr::bind_rows(samples_cases_test, samples_cases_hosp, samples_deaths) %>%
  #truncate
  dplyr::filter((source %in% c("cases_test", "cases_hosp") & date < trunc_cases) |
                (source == "deaths_death" & date < trunc_deaths))

saveRDS(samples, "rt-estimate/samples_truncated.rds")
