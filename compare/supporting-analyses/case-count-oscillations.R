# Compare case counts and oscillations
library(dplyr); library(magrittr)
source("utils/utils.R")

summary <- readRDS("rt-estimate/estimate-all-time/summary_truncated.rds")
peaks_troughs <- readRDS("compare/rt-comparison/peaks_troughs.rds")

# Get count data ----------------------------------------------------------
# If data won't download, read in a saved hard copy of cleaned data 
if(class(try(source(here::here("data", "get-uk-data.R")))) == "try-error") {
  message("--- API failure - loading saved data ---")
  data <- readRDS(data_save)
} else {
  source(here::here("data", "get-uk-data.R"))
}

rt_data <- tidyr::pivot_longer(data, -c(date, region), names_to = "source", values_to = "data") %>%
  dplyr::filter(!source=="cases_hosp_new") %>%
  dplyr::left_join(summary %>% dplyr::select(date, region, source, 
                                             median, lower_90, upper_90), 
                   by = c("date", "region", "source")) %>%
  dplyr::filter(date %in% summary$date) %>%
  dplyr::group_by(region, source) %>%
  dplyr::mutate(diff_median = median - dplyr::lag(median, 1),
                abs_diff_median = abs(median - dplyr::lag(median, 1)))



# Data count vs time between valleys --------------------------------------
# Join time between variable
# source("rt-comparison/wave-features.R")
rt_data <- rt_data %>%
  left_join(peaks_troughs$time_between_valleys %>% 
              ungroup() %>% 
              select(region, date, source, valley_adj, time_between_valleys),
            by = c("region", "date", "source")) %>%
  ungroup()


# Average of count in days between Rt valleys
rt_data_mean <- rt_data %>%
  group_by(region, source) %>%
  tidyr::fill(time_between_valleys) %>%
  group_by(time_between_valleys, source, region) %>%
  summarise(mean_data = mean(data)) %>%
  filter(!region == "England")

# Linear model
fit <- rt_data_mean %>%
  mutate(time_between_valleys = as.numeric(time_between_valleys)) %>%
  tidyr::drop_na()
fit <- split(fit, fit$source) %>%
  purrr::map(., ~ lm(time_between_valleys ~ mean_data, data = .x))

#  Slope
purrr::map(fit, ~ signif(.x$coef[[2]], 5))
#  R2
signif(summary(fit$cases_hosp)$adj.r.squared, 5)
signif(summary(fit$cases_test)$adj.r.squared, 5)
signif(summary(fit$deaths_death)$adj.r.squared, 5)
# F stat
purrr::map(fit, ~ summary(.x)$fstatistic[1])

# Plot
rt_data_mean %>%
  mutate(time_between_valleys = as.numeric(time_between_valleys),
         `Data source` = factor(source, 
                                       levels = c("cases_test", "cases_hosp", "deaths_death"),
                                       labels = c("Test positive cases", "Hospital admissions", "Deaths"))) %>%
  ggplot(aes(x = mean_data, y = time_between_valleys, 
             fill = `Data source`)) +
  geom_point(aes(colour = `Data source`)) +
  stat_smooth(method = "lm", colour = "grey 50") +
  annotate("text", x = 450, y = 60, 
           label = paste("italic(R) ^ 2 == ", 
                         round(signif(summary(fit$cases_test)$adj.r.squared, 5),2)),
           parse = TRUE, colour = "dark red") +
  annotate("text", x = 445, y = 55, 
           label = paste("italic(R) ^ 2 == ", 
                         round(signif(summary(fit$cases_hosp)$adj.r.squared, 5),2)),
           parse = TRUE, colour = "dark green") +
  annotate("text", x = 450, y = 50, 
           label = paste("italic(R) ^ 2 == ", 
                         round(signif(summary(fit$deaths_death)$adj.r.squared, 5),2)),
           parse = TRUE, colour = "dark blue") +
  labs(x = "Mean count of data over oscillation", 
  y = "Length of Rt oscillation 
       (Days between valleys in median Rt)") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        text = element_text(size = 8)) +
  ggsave("figures/rt-oscillation-by-counts.png", height = 4, width = 5)



