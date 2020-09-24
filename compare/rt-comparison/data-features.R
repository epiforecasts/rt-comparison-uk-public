# Raw case count features
source("data/get-uk-data.R")

data <- as.data.frame(data)

data_regional <- split(data, data$region)

# Early peaks
early_peak <- data_regional %>%
  purrr::map(., ~ dplyr::filter(., date <= as.Date("2020-06-01")))

early_peak <- data %>%
  tidyr::pivot_wider(names_from = region, values_from = deaths_death)

which.max(data_regional$`East of England`$deaths_death)

# Mean difference between counts
data_diffs <- purrr::map(data_regional, ~ 
                          dplyr::mutate(., 
                                        diff_case_death = .$cases_test - .$deaths_death,
                                        diff_case_hosp = .$cases_test - .$cases_hosp,
                                        diff_hosp_death = .$cases_hosp - .$deaths_death,
                                        index = seq_len(nrow(.))) %>%
                          dplyr::filter(date <= as.Date("2020-08-10")))

data_mean <- data_diffs %>%
  purrr::keep(names(.) %in% region_names$nhsregions) %>%
  purrr::transpose() %>%
  purrr::keep(names(.) %in% c("diff_case_death", "diff_case_hosp", "diff_hosp_death")) %>%
  purrr::map_depth(.depth = 2, t.test, na.rm=T) %>%
  purrr::map_depth(.depth = 2, ~ purrr::keep(., names(.) %in% c("estimate"))) %>%
  purrr::map_depth(.depth = 2, ~ dplyr::bind_rows(.)) %>%
  dplyr::bind_rows(., .id = "id")

data_mean_min <- tidyr::pivot_longer(data_mean, cols = -id, values_to = "mean") %>%
  dplyr::group_by(id) %>%
  dplyr::filter(mean == min(mean))

data_mean_max <- tidyr::pivot_longer(data_mean, cols = -id, values_to = "mean") %>%
  dplyr::group_by(id) %>%
  dplyr::filter(mean == max(mean))

# Waves

source("compare/wave-features.R")

data_wave <- list(
  cases = purrr::map(data_regional, ~ wave_features(.x, "cases_test", 7)),
  hosp = purrr::map(data_regional, ~ wave_features(.x, "cases_hosp", 7)),
  deaths = purrr::map(data_regional, ~ wave_features(.x, "deaths_death", 7)))

data_wave <- data_wave %>%
  purrr::transpose() %>%
  purrr::discard(.p = names(.) %in% c("Scotland", "Wales", "Northern Ireland"))



# Test positivity ---------------------------------------------------------

t.test(pos_tests[pos_tests$region == "Midlands", "pos_perc"])


