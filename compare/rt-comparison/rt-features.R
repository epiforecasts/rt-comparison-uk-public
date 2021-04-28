# General Rt comparison
library(magrittr); library(ggplot2)

# Set up ------------------------------------------------------------------
source("utils/utils.R")

summary <- readRDS("rt-estimate/estimate-all-time/summary_truncated.rds")
summary <- dplyr::mutate(summary,
                         week = lubridate::week(date),
                         month = lubridate::month(date))

summary_source_region <- split(summary, summary$source)
summary_source_region <- purrr::map(summary_source_region, ~ split(.x, .x$region))

# First peak -------------------------------------------------------------------------
early_peak <- summary %>%
  select(-type) %>%
  filter(date < "2020-05-01") %>%
  group_by(source, region) %>%
  slice_max(median)


# General time series features -------------------------------------------------------
# See all TS features
tsfeats <- purrr::map_depth(.depth = 2,
                            summary_source_region, 
                            ~ tsfeatures::tsfeatures(.x[["median"]])) %>%
  purrr::map(., ~ dplyr::bind_rows(., .id = "region")) %>%
  dplyr::bind_rows(., .id = "source")


# Average differences -------------------------------------------------------------------
summary_wide <- summary %>%
  select(-type) %>%
  tidyr::pivot_wider(id_cols = c(date, region),
                     names_from = source, values_from = c(lower_90:median)) %>%
  mutate(median_cases_deaths = median_cases_test / median_deaths_death,
         median_cases_admissions = median_cases_test / median_cases_hosp,
         median_admissions_deaths = median_cases_hosp / median_deaths_death) 

median_cases_deaths <- summary_wide %>%
  split(summary_wide$region) %>%
  purrr::map( ~ t.test(.x$median_cases_deaths)) %>%
  purrr::map(., ~ purrr::keep(., names(.) %in% c("estimate", "conf.int")))

median_cases_admissions <- summary_wide %>%
  split(summary_wide$region) %>%
  purrr::map( ~ t.test(.x$median_cases_admissions)) %>%
  purrr::map(., ~ purrr::keep(., names(.) %in% c("estimate", "conf.int")))

median_admissions_deaths <- summary_wide %>%
  split(summary_wide$region) %>%
  purrr::map( ~ t.test(.x$median_admissions_deaths)) %>%
  purrr::map(., ~ purrr::keep(., names(.) %in% c("estimate", "conf.int")))









