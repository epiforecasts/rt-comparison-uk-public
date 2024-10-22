require(dplyr)
require(ggplot2)
require(rvest)
source("compare/supporting-analyses/test-positivity.R")


# Get count data ----------------------------------------------------------
# If data won't download, read in a saved hard copy of cleaned data - 23 August 2020
if (class(try(source(here::here("data", "get-uk-data.R")))) == "try-error") {
  message("--- API failure - loading saved data ---")
  data <- readRDS(data_save)
} else {
  source(here::here("data", "get-uk-data.R"))
}

# Get region names and plotting colours
source("utils/utils.R")

# Global vars for plots in plot-all.R
if (!exists("date_min")) {
  # consistent date axis:
  date_min <- as.Date("2020-04-04")
  date_max <- max(summary$date)
  # theme
  theme_set(theme_classic(base_size = 15))
}


# Standardise ----------------------------------------------------------------

standardised_data <- data %>%
  # Filter before data truncation (where Rt is "estimate" not "based on partial data")
  dplyr::filter(date >= date_min & date <= date_max) %>%
  # Standardise by region
  dplyr::group_by(region) %>%
  #  - z-scores
  dplyr::mutate(# 7-day moving average
                ma_cases_test = forecast::ma(cases_test, order = 7), 
                ma_cases_hosp = forecast::ma(cases_hosp, order = 7), 
                ma_deaths_death = forecast::ma(deaths_death, order = 7)) %>% 
  dplyr::ungroup() 

# Factor regions for consistent plot alignment
standardised_data$region = factor(standardised_data$region, 
                               levels = region_names$region_factor)


# Plot-ready 7-day MA ----------------------------------------------------------------
data_ma <- standardised_data %>%
  dplyr::select(date, region, dplyr::starts_with("ma")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("ma"), names_to = "variable", values_to = "ma") %>%
  dplyr::mutate(variable = stringr::str_remove_all(variable, "^ma_")) %>%
  dplyr::mutate('Data source' = dplyr::recode_factor(variable, 
                                                     "cases_test" = "Cases",
                                                     "cases_hosp" = "Hospital admissions",
                                                     "deaths_death" = "Deaths")) %>%
  # Add positivity rates (weekly)
  dplyr::left_join(pos_tests, by = c("date", "region")) %>%
  dplyr::mutate(pos_perc = factor(ifelse(!`Data source` == "Cases", NA, 
                                         ifelse(pos_perc < 4.945 |
                                                is.na(pos_perc), NA, ma))),
                week = lubridate::week(date)) %>%
  dplyr::group_by(region, week, `Data source`) %>%
  tidyr::fill(pos_perc, .direction = "updown") %>%
  dplyr::mutate(pos_perc = ifelse(is.na(pos_perc), NA, ma),
                region = factor(region, levels = region_names$region_factor))

# Plot --------------------------------------------------------------------

# Plot function
plot_data_fn <- function(region_name){
  data_ma %>%
    dplyr::filter(region %in% region_name) %>%
    ggplot() +
    geom_line(aes(x = date, y = as.numeric(ma), 
                  colour = `Data source`)) +
    geom_point(aes(x = date, y = as.numeric(pos_perc),
                   colour = `Data source`), 
               shape = 3, size = 0.9) +
    scale_shape_discrete(solid = FALSE) +
    geom_vline(xintercept = as.Date("2020-05-03"), 
               lty = 3, colour = colours["Cases"]) +
    cowplot::theme_cowplot() +
    # coord_cartesian(xlim = c(date_min, date_max)) +
    scale_color_manual(values = colours) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    theme(panel.spacing.x = unit(0.1, "cm"),
          panel.spacing.y = unit(0.1, "cm")) +
    guides(colour = FALSE) +
    labs(y = "7-day MA", x = NULL, subtitle = region_name)
}
