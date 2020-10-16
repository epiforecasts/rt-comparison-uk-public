# General Rt comparison
library(magrittr); library(ggplot2); library(dplyr)
library(patchwork)

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


# Rt crossing 1: end of first wave -----------------------------------------------------------------
rt_under1 <- summary %>%
  dplyr::group_by(region, source) %>%
  dplyr::filter(date <= "2020-07-01") %>%
  dplyr::group_by(region, source) %>%
  dplyr::mutate(over1 = ifelse(median < 1, TRUE, NA),
                over1_10 = ifelse(
                  over1==TRUE & lead(over1, 1) & lead(over1, 2) & lead(over1, 3) & lead(over1, 4) & lead(over1, 5) & lead(over1, 6), 
                  TRUE, NA)) %>%
  dplyr::filter(over1_10 == TRUE) %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(source, region, rt = date) %>%
  dplyr::left_join(summary %>%
                     dplyr::group_by(region, source) %>%
                     dplyr::filter(date <= "2020-07-01") %>%
                     dplyr::group_by(region, source) %>%
                     dplyr::mutate(over1 = ifelse(lower_90 < 1, TRUE, NA),
                                   over1_10 = ifelse(
                                     over1==TRUE & lead(over1, 1) & lead(over1, 2) & lead(over1, 3) & lead(over1, 4) & lead(over1, 5) & lead(over1, 6), 
                                     TRUE, NA)) %>%
                     dplyr::filter(over1_10 == TRUE) %>%
                     dplyr::filter(date == min(date)) %>%
                     dplyr::select(source, region, rt = date),
                   by = c("region", "source")) %>%
  dplyr::left_join(summary %>%
                     dplyr::group_by(region, source) %>%
                     dplyr::filter(date <= "2020-07-01") %>%
                     dplyr::group_by(region, source) %>%
                     dplyr::mutate(over1 = ifelse(upper_90 < 1, TRUE, NA),
                                   over1_10 = ifelse(
                                     over1==TRUE & lead(over1, 1) & lead(over1, 2) & lead(over1, 3) & lead(over1, 4) & lead(over1, 5) & lead(over1, 6), 
                                     TRUE, NA)) %>%
                     dplyr::filter(over1_10 == TRUE) %>%
                     dplyr::filter(date == min(date)) %>%
                     dplyr::select(source, region, rt = date),
                   by = c("region", "source")) %>%
  dplyr::rename(median = rt.x,
                lower = rt.y,
                upper = rt) %>%
  dplyr::mutate(region = factor(region, levels = region_names$region_factor))





# Later Rt >1  ------------------------------------------------------------

# Start of second wave of sustained transmission: 
# Earliest date where Rt rose > 1 for 3+ successive days
# 
rt_over1 <- summary %>%
  dplyr::filter(date > "2020-05-01") %>%
  dplyr::group_by(region, source) %>%
  dplyr::mutate(over1 = ifelse(median > 1, TRUE, NA),
                over1_10 = ifelse(
                  over1==TRUE & lead(over1, 1) & lead(over1, 2) & lead(over1, 3) & lead(over1, 4) & lead(over1, 5) & lead(over1, 6), 
                  TRUE, NA)) %>%
  dplyr::filter(over1_10 == TRUE) %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(source, region, rt = date) %>%
  dplyr::left_join(summary %>%
                     dplyr::filter(date > "2020-05-01") %>%
                     dplyr::group_by(region, source) %>%
                     dplyr::mutate(over1 = ifelse(lower_90 > 1, TRUE, NA),
                                   over1_10 = ifelse(
                                     over1==TRUE & lead(over1, 1) & lead(over1, 2) & lead(over1, 3) & lead(over1, 4) & lead(over1, 5) & lead(over1, 6),
                                     TRUE, NA)) %>%
                     dplyr::filter(over1_10 == TRUE) %>%
                     dplyr::filter(date == min(date)) %>%
                     dplyr::select(source, region, rt = date),
                   by = c("region", "source")) %>%
  dplyr::left_join(summary %>%
                     dplyr::filter(date > "2020-05-01") %>%
                     dplyr::group_by(region, source) %>%
                     dplyr::mutate(over1 = ifelse(upper_90 > 1, TRUE, NA),
                                   over1_10 = ifelse(
                                     over1==TRUE & lead(over1, 1) & lead(over1, 2) & lead(over1, 3) & lead(over1, 4) & lead(over1, 5) & lead(over1, 6), 
                                     TRUE, NA)) %>%
                     dplyr::filter(over1_10 == TRUE) %>%
                     dplyr::filter(date == min(date)) %>%
                     dplyr::select(source, region, rt = date),
                   by = c("region", "source")) %>%
  dplyr::rename(median = rt.x,
                lower = rt.y,
                upper = rt) %>%
  dplyr::mutate(region = factor(region, levels = region_names$region_factor))






# Combine plots -----------------------------------------------------------

# Plot
rt_under1_plot <- rt_under1 %>%
  dplyr::mutate(source = factor(source, 
                                levels = c("cases_test", "cases_hosp", "deaths_death"),
                                labels = c("Test-positive", "Hospital admissions", "Deaths"))) %>%
  dplyr::rename(`Data source` = source) %>%
  ggplot(groups = region, colour = `Data source`) +
  geom_point(aes(x = median, y = region, colour = `Data source`,
                 shape = `Data source`), size = 2) +
  geom_linerange(aes(y = region, xmin = lower, xmax = upper, 
                     colour = `Data source`)) +
  geom_vline(aes(xintercept = as.Date("2020-03-23")), lty = 2, colour = "grey 50") +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values = colours) +
  theme_classic() +
  theme(legend.position = "bottom")


# Plot
rt_over1_plot <- rt_over1 %>%
  dplyr::mutate(source = factor(source, 
                                levels = c("cases_test", "cases_hosp", "deaths_death"),
                                labels = c("Test-positive", "Hospital admissions", "Deaths"))) %>%
  dplyr::rename(`Data source` = source) %>%
  ggplot(groups = region, colour = `Data source`) +
  geom_point(aes(x = median, y = region, colour = `Data source`,
                 shape = `Data source`), size = 2) +
  geom_linerange(aes(y = region, 
                     xmin = median,
                     xmax = upper, 
                     colour = `Data source`)) +
  geom_linerange(aes(y = region, 
                     xmin = lower,
                     xmax = median, 
                     colour = `Data source`)) +
  geom_vline(aes(xintercept = as.Date("2020-07-03")), lty = 2, colour = "grey 50") +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values = colours) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text.y = element_blank())


rt_under1_plot +
  rt_over1_plot + 
  plot_layout(guides = "collect") &
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")


#######
rt1 <- bind_rows(rt_over1, rt_under1, .id = "phase") %>%
  dplyr::mutate(source = factor(source, 
                                levels = c("cases_test", "cases_hosp", "deaths_death"),
                                labels = c("Test positive cases", "Hospital admissions", "Deaths"))) %>%
  dplyr::rename(`Data source` = source) %>%
  ggplot(groups = c(region, colour), colour = `Data source`) +
  geom_point(aes(x = median, y = region, colour = `Data source`,
                 shape = `Data source`), size = 2) +
  geom_linerange(aes(y = region, 
                     xmin = median,
                     xmax = upper, 
                     colour = `Data source`)) +
  geom_linerange(aes(y = region, 
                     xmin = lower,
                     xmax = median, 
                     colour = `Data source`)) +
  scale_color_manual(values = colours) +
  geom_vline(aes(xintercept = as.Date("2020-07-03")), lty = 2, colour = "grey 50") +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(legend.position = "bottom")



ggsave(paste0("figures/", Sys.Date(), "-rt-cross-1.png"), height = 3, width = 7)


