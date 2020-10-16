# SI table 1
library(data.table)
library(dplyr)
library(tidyr)

source("utils/utils.R")
summary <- readRDS("rt-estimate/estimate-all-time/summary_truncated.rds")
peaks_troughs <- readRDS("compare/rt-comparison/peaks_troughs.rds")


# Date median first crossed 1	------------------
rt1 <- summary %>%
  dplyr::group_by(region, source) %>%
  dplyr::filter(median < 1 & date > "2020-03-23" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::mutate(value = format(date, c("%d %B")),
                name = "rt1") %>%
  dplyr::select(source, region, date, value, name)

summary_after_1 <- left_join(summary, rt1, by = c("source", "region", "date")) %>%
  group_by(source, region) %>%
  fill(value, .direction = "down") %>%
  filter(!is.na(value))


# Series median and 90% CrI	 -----------
# date after median crossed 1 - last date of estimate
# Using dtplyr
library(dtplyr)
# Using raw output samples to avoid taking a median of a median
samples <- readRDS("rt-estimate/samples_truncated.rds")
samples <- lazy_dt(samples)
samples <- select(samples, source, region, date, sample, value)

# Filter to date > Rt cross 1 
median_after_1 <- samples %>%
  group_by(region, source) %>%
  left_join(rt1, by = c("source", "region", "date")) %>%
  as_tibble() %>%
  group_by(region, source) %>%
  tidyr::fill(name) %>%
  filter(!is.na(name)) %>%
  group_by(region, source) %>%
  summarise(median = median(value.x),
            lower_90 = quantile(value.x, 0.1),
            upper_90 = quantile(value.x, 0.9)) %>%
  as_tibble()
  
median_after_1 <- median_after_1 %>%
  mutate(value = paste0(round(median, 2), 
                        " (", round(lower_90, 2), 
                        "-", round(upper_90, 2), ")"),
         name = "median_after_1") %>%
  select(region, source, value, name)
  
detach("package:dtplyr", character.only = TRUE, unload = TRUE)
library(dplyr, warn.conflicts = TRUE)

# Number of times a single median estimate crosses the series median	----------------
summary_source_region <- split(summary, summary$source)
summary_source_region <- purrr::map(summary_source_region, ~ split(.x, .x$region))
cross_points <- purrr::map_depth(.depth = 2,
                                 summary_source_region, 
                                 ~ tsfeatures::crossing_points(.x[["median"]])) %>%
  dplyr::bind_rows(.id = "region") %>%
  t() %>%
  janitor::row_to_names(1) %>%
  tibble::as_tibble(rownames = "region") %>%
  dplyr::mutate(dplyr::across(2:4, as.numeric)) %>%
  tidyr::pivot_longer(-region, names_to = "source", values_to = "value") %>%
  dplyr::mutate(value = as.character(value),
                name = "crossing_points")

# Minimum and maximum median estimate in time series (date, median, 90%CrI)	------------
min_after_1 <- summary_after_1 %>%
  group_by(region, source) %>%
  slice_min(order_by = median) %>%
  mutate(value = paste0(format(date, c("%d %b")), 
                      " (", round(median, 2), 
                      " [",round(lower_90, 2), 
                      "-", round(upper_90, 2), "])"),
         name = "min_after_1") %>%
  select(region, source, value, name)

max_after_1 <- summary_after_1 %>% 
  group_by(region, source) %>%
  slice_max(order_by = median) %>%
  mutate(value = paste0(format(date, c("%d %b")), 
                      " (", round(median, 2), 
                      " [",round(lower_90, 2), 
                      "-", round(upper_90, 2), "])"),
         name = "max_after_1") %>%
  select(region, source, value, name)


# Duration of days between successive troughs (mean, 95% CI) ------------
duration_valleys <- peaks_troughs[["time_between_valleys"]] %>%
  ungroup() %>%
  drop_na(time_between_valleys) %>%
  group_by(region, source) %>%
  summarise(n = n(),
            mean = mean(time_between_valleys),
            se = qnorm(0.95)*(sd(time_between_valleys/sqrt(n))),
            lo95 = mean - se,
            hi95 = mean + se) %>%
  mutate(value = ifelse(n > 1, paste0(round(mean), 
                                 " (", round(lo95), 
                                 "-", round(hi95), 
                                 ", n=", n, ")"),
                        paste0(round(mean, 2), " (n=1)")),
         region = factor(region, levels = region_names$region_factor),
         name = "duration_valleys") %>%
  select(region, source, value, name)


# Join into table ---------------------------------------------------------
source_factor <- c("Test-positive cases" = "cases_test", 
            "Hospital admissions" = "cases_hosp", 
            "Deaths" = "deaths_death")

table1 <- bind_rows(rt1, 
                    median_after_1, min_after_1, max_after_1, 
                    cross_points, duration_valleys) %>%
  select(-date) %>%
  pivot_wider(id_cols = c(source, region), 
              names_from = name,
              values_from = value) %>%
  mutate(region = factor(region, levels = region_names$region_factor),
         source = factor(source, levels = source_factor, labels = names(source_factor))) %>%
  select(region, source, everything()) %>%
  group_by(source) %>%
  arrange(region, .by_group = TRUE) %>%
  group_by(region) %>%
  arrange(source, .by_group = TRUE)

names(table1) <- c("Region",
                   "Data source",
                   "Earliest date median Rt <1",
                   "Median Rt after first wave* (90% CrI)",
                   "Minimum median Rt after first wave* (date, median [90%CrI])",
                   "Maximum median Rt after first wave* (date, median [90%CrI])",
                   "Number of days where median Rt crossed the series median",
                   "Mean number of days between troughs** (mean, 95% CI)")

table1 <- table1 %>%
  ungroup() %>%
  add_row(Region = "90%CrI = credible interval (quantiles around median); 95%CI = confidence interval (standard error around mean)") %>%
  add_row(Region = "*End of first wave = earliest date Rt <1. Last estimate of Rt from cases and admissions: 27 August, from deaths: 19th August 2020") %>%
  add_row(Region = "**Troughs defined as local minima within a sequential fall and rise in the median Rt estimate")


readr::write_csv(table1, paste0("compare/rt-comparison/", Sys.Date(), "-si-table-1.csv"))

