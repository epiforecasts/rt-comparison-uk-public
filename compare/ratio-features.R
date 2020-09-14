# Ratio summary statistics
summary_wide <- readRDS("rt-estimate/summary_wide.rds")

library(dplyr)

summary_ratio <- summary_wide
#summary_ratio <- dplyr::filter(summary_ratio, date >= as.Date("2020-06-26"))
summary_ratio <- split(summary_wide, summary_wide$region) 

caseb_deathb_med <- summary_ratio %>%
  purrr::map( ~ t.test(.x$caseb_deathb_med)) %>%
  purrr::transpose() %>%
  purrr::keep(., .p = names(.) %in% c("estimate", "conf.int")) %>%
  purrr::transpose()

caseb_hosp_med <- summary_ratio %>%
  purrr::map( ~ t.test(.x$caseb_hosp_med)) %>%
  purrr::transpose() %>%
  purrr::keep(., .p = names(.) %in% c("estimate", "conf.int")) %>%
  purrr::transpose()

hosp_deathb_med <- summary_ratio %>%
  purrr::map( ~ t.test(.x$hosp_deathb_med)) %>%
  purrr::transpose() %>%
  purrr::keep(., .p = names(.) %in% c("estimate", "conf.int")) %>%
  purrr::transpose()


median <- dplyr::select(summary_wide, date, region, dplyr::starts_with("median"))
lower90 <- dplyr::select(summary_wide, date, region, dplyr::starts_with("lower_90"))
upper90 <- dplyr::select(summary_wide, date, region, dplyr::starts_with("upper_90"))
