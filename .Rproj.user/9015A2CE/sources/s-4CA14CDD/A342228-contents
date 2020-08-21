require(dplyr)
require(ggplot2)

# Get max date to filter estimates to
max_date <- readRDS("rt-estimate/max_data_date.rds")

# Get count data ----------------------------------------------------------
source("data/get-uk-data.R")

# Standardise ----------------------------------------------------------------

standardised_data <- data %>%
  # Filter before data truncation (where Rt is "estimate" not "based on partial data")
  dplyr::filter(date >= (max(date) - lubridate::weeks(12)) & date <= max_date) %>%
  # Standardise by region
  dplyr::group_by(region) %>%
  #  - z-scores
  dplyr::mutate(zcases_test = scale(cases_test, center = TRUE, scale = TRUE), 
                zcases_publish = scale(cases_publish, center = TRUE, scale = TRUE), 
                zcases_hosp = scale(cases_hosp, center = TRUE, scale = TRUE), 
                zdeaths = scale(deaths, center = TRUE, scale = TRUE),
   #  - 7-day moving average
                ma_cases_test = forecast::ma(cases_test, order = 7), 
                ma_cases_publish = forecast::ma(cases_publish, order = 7),
                ma_cases_hosp = forecast::ma(cases_hosp, order = 7), 
                ma_deaths = forecast::ma(deaths, order = 7)) %>% 
  dplyr::ungroup()

# Plot-ready z-scores ----------------------------------------------------------------
data_zcore <- standardised_data %>%
  dplyr::select(date, region, region_type, dplyr::starts_with("z")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("z"), names_to = "variable", values_to = "zscore") %>%
  dplyr::mutate(variable = stringr::str_remove_all(variable, "^z")) %>%
  dplyr::mutate('Data source' = dplyr::recode_factor(variable, 
                                                     "cases_test" = "Cases by test date",
                                                     "cases_publish" = "Cases by report date",
                                                     "cases_hosp" = "Hospital admissions",
                                                     "deaths" = "Deaths")) %>%
  dplyr::filter(region_type == "nation")

# Plot-ready 7-day MA ----------------------------------------------------------------
data_ma <- standardised_data %>%
  dplyr::select(date, region, region_type, dplyr::starts_with("ma")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("ma"), names_to = "variable", values_to = "ma") %>%
  dplyr::mutate(variable = stringr::str_remove_all(variable, "^ma_")) %>%
  dplyr::mutate('Data source' = dplyr::recode_factor(variable, 
                                                     "cases_test" = "Cases by test date",
                                                     "cases_publish" = "Cases by report date",
                                                     "cases_hosp" = "Hospital admissions",
                                                     "deaths" = "Deaths")) %>%
  dplyr::filter(region_type == "nation")
  

# National -------------------------------------------------------------

# Plot 7 day MA
plot_national_ma <- data_ma %>%
  ggplot() +
  geom_line(aes(x = date, y = as.numeric(ma), colour = `Data source`)) +
  facet_wrap(~ region, nrow = 1, scales = "free_y") +
  cowplot::theme_cowplot(font_size = 11) +
  #scale_color_manual(values = colours) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  #theme(legend.position = "none") +
  #theme(axis.text.x = element_blank()) +
  labs(title = "Smoothed counts", y = "Count, 7-day moving average", x = "")

# plot(plot_count_national)

# Plot zscore
# plot_national_zscore <- data_zscore %>%
#   ggplot() +
#   geom_line(aes(x = date, y = as.numeric(zscore), colour = `Data source`)) +
#   #geom_hline(yintercept = 0, linetype = 2) +
#   facet_wrap(~ region, nrow = 1, scales = "free_y") +
#   cowplot::theme_cowplot(font_size = 11) +
#   #scale_color_manual(values = colours) +
#   theme(panel.spacing.x = unit(0.5, "cm")) +
#   #theme(legend.position = "none") +
#   #theme(axis.text.x = element_blank()) +
#   labs(title = "Standardised raw counts", y = "z-score", x = "")

