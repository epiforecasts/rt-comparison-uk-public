# General Rt comparison
library(magrittr); library(ggplot2)

# Set up ------------------------------------------------------------------
source("utils/utils.R")

summary <- readRDS("rt-estimate/summary.rds")
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


# Rt crossing 1: end of first wave -----------------------------------------------------------------
# Check timing of first wave cross below 1
source("utils/utils.R")
summary <- readRDS("rt-estimate/estimate-all-time/summary_truncated.rds")

# Cases
rt1 <- summary %>%
  dplyr::group_by(region, source) %>%
  dplyr::filter(median < 1  & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(source, region, rt = date) %>%
  dplyr::left_join(summary %>%
                     dplyr::group_by(region, source) %>%
                     dplyr::filter(lower_90 < 1 & date <= "2020-07-01") %>%
                     dplyr::filter(date == min(date)) %>%
                     dplyr::select(source, region, rt = date),
                   by = c("region", "source")) %>%
  dplyr::left_join(summary %>%
                     dplyr::group_by(region, source) %>%
                     dplyr::filter(upper_90 < 1 & date <= "2020-07-01") %>%
                     dplyr::filter(date == min(date)) %>%
                     dplyr::select(source, region, rt = date),
                   by = c("region", "source")) %>%
  dplyr::rename(median = rt.x,
                lower = rt.y,
                upper = rt) %>%
  dplyr::mutate(region = factor(region, levels = region_names$region_factor))


rt1_plot <- rt1 %>%
  dplyr::mutate(source = factor(source, 
                                levels = c("cases_test", "cases_hosp", "deaths_death"),
                                labels = c("Test-positive", "Hospital admissions", "Deaths"))) %>%
  dplyr::rename(`Data source` = source) %>%
  ggplot(groups = region, colour = `Data source`) +
  geom_point(aes(x = median, y = region, colour = `Data source`,
                 shape = `Data source`), size = 2) +
  geom_linerange(aes(y = region, xmin = lower, xmax = upper, 
                     colour = `Data source`)) +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values = colours) +
  theme_classic() +
  theme(legend.position = "bottom") +
  ggsave(paste0("figures/", Sys.Date(), "-rt-cross-1.png"),
         height = 2, width = 6)



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









