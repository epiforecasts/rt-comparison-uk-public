# Rt crossing 1: end of first wave -----------------------------------------------------------------
# Check timing of first wave cross below 1
library(dplyr); library(ggplot2)

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
  ggsave(paste0("figures/", Sys.Date(), "-RSTB20200283-figure-2.pdf"),
         dpi = "print",  height = 2, width = 6)
