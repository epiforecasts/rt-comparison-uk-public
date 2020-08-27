require(dplyr)
require(ggplot2)

# Get count data ----------------------------------------------------------
# If data won't download, read in a saved hard copy of cleaned data - 23 August 2020
if(class(try(source(here::here("data", "get-uk-data.R")))) == "try-error") {
  message("--- API failure - loading saved data ---")
  data <- readRDS("data/200823.rds")
} else {
  source(here::here("data", "get-uk-data.R"))
}

# Get Rt estimate dates
min_date <- readRDS("utils/earliest_estimate.rds")
max_date <- readRDS("utils/latest_estimate.rds")

# Get region names and plotting colours
source("utils/utils.R")

# Standardise ----------------------------------------------------------------

standardised_data <- data %>%
  # Filter before data truncation (where Rt is "estimate" not "based on partial data")
  dplyr::filter(date >= min_date & date <= max_date) %>%
  # Standardise by region
  dplyr::group_by(region) %>%
  #  - z-scores
  dplyr::mutate(zcases_blend = scale(cases_blend, center = TRUE, scale = TRUE), 
                zcases_hosp = scale(cases_hosp, center = TRUE, scale = TRUE), 
                zdeaths_blend = scale(deaths_blend, center = TRUE, scale = TRUE),
   #  - 7-day moving average
                ma_cases_blend = forecast::ma(cases_blend, order = 7), 
                ma_cases_hosp = forecast::ma(cases_hosp, order = 7), 
                ma_deaths_blend = forecast::ma(deaths_blend, order = 7)) %>% 
  dplyr::ungroup() 

# Factor regions for consistent plot alignment
standardised_data$region = factor(standardised_data$region, 
                               levels = region_names$region_factor)


# Plot-ready z-scores ----------------------------------------------------------------
data_zscore <- standardised_data %>%
  dplyr::select(date, region, region_type, dplyr::starts_with("z")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("z"), names_to = "variable", values_to = "zscore") %>%
  dplyr::mutate(variable = stringr::str_remove_all(variable, "^z")) %>%
  dplyr::mutate('Data source' = dplyr::recode_factor(variable, 
                                                     "cases_blend" = "Cases",
                                                     "cases_hosp" = "Hospital admissions",
                                                     "deaths_blend" = "Deaths"))

# Plot-ready 7-day MA ----------------------------------------------------------------
data_ma <- standardised_data %>%
  dplyr::select(date, region, region_type, dplyr::starts_with("ma")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("ma"), names_to = "variable", values_to = "ma") %>%
  dplyr::mutate(variable = stringr::str_remove_all(variable, "^ma_")) %>%
  dplyr::mutate('Data source' = dplyr::recode_factor(variable, 
                                                     "cases_blend" = "Cases",
                                                     "cases_hosp" = "Hospital admissions",
                                                     "deaths_blend" = "Deaths"))


# Plot --------------------------------------------------------------------

# Plot 7 day MA
plot_ma <- data_ma %>%
  ggplot() +
  geom_line(aes(x = date, y = as.numeric(ma), colour = `Data source`)) +
  facet_wrap("region", nrow = 1) + #, scales = "free_y"
  cowplot::theme_cowplot() +
  coord_cartesian(xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(legend.position="top", legend.box = "horizontal") +
  theme(axis.text.x = element_blank()) +
  labs(subtitle = "Smoothed counts", y = "Centred 7-day MA", x = "")

# Plot zscore
plot_zscore <- data_zscore %>%
  ggplot() +
  geom_line(aes(x = date, y = as.numeric(zscore), colour = `Data source`)) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_cartesian(xlim = c(date_min, date_max)) +
  facet_wrap("region", nrow = 1) +
  cowplot::theme_cowplot() +
  scale_color_manual(values = colours) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(axis.text.x = element_blank()) +
  labs(subtitle = "Standardised raw counts", y = "z-score", x = "")

