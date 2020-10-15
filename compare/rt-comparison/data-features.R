# Raw case count features
source("data/get-uk-data.R")

# Early peaks
data_peaks <- data %>%
  filter(date < "2020-06-01") %>%
  select(-cases_hosp_new) %>%
  tidyr::pivot_longer(-c(date, region), names_to = "source", values_to = "count") %>%
  dplyr::group_by(region, source) %>%
  dplyr::slice_max(n = 1, order_by = "count")
                

