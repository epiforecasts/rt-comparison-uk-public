# Packages ----------------------------------------------------------------

require(data.table, quietly = TRUE)
require(EpiNow2, quietly = TRUE)
require(dplyr)
require(tidyr)
require(purrr)

# Get national -------------------------------------------------------------

estimates <- list(#"cases_test",
                  "cases_publish",
                  "cases_hosp",
                  "deaths")
names(estimates) <- estimates

# # Get full samples
# results <- estimates %>%
#   purrr::map( ~ EpiNow2::get_regional_results(
#                       results_dir = paste0(here::here("rt-estimate/estimate", ., "region")))) %>%
#   purrr::map_dfr(~ .x$estimates$samples, .id = "source")

# Get Rt only
summary <- estimates %>%
  purrr::map(~ readr::read_csv(paste0(here::here("rt-estimate", "estimate"), "/", ., "/summary/rt.csv"))) %>%
  purrr::map_dfr(~ .x, .id = "source") %>%
  # Filter out "estimate based on partial data"
  dplyr::filter(type == "estimate")

max_date <- max(summary$date)
saveRDS(max_date, file = "rt-estimate/max_data_date.rds")

# Take ratios -------------------------------------------------------------

summary_wide <- tidyr::pivot_wider(summary, 
                                   names_from = source, 
                                   values_from = c("median","lower_90", "upper_90", 
                                                   "lower_50", "upper_50")) %>%
  dplyr::mutate(
    # cases_publish / cases_hosp
    pub_hosp_med = median_cases_publish / median_cases_hosp,
    pub_hosp_l90 = lower_90_cases_publish / lower_90_cases_hosp,
    pub_hosp_u90 = upper_90_cases_publish / upper_90_cases_hosp,
    pub_hosp_l50 = lower_50_cases_publish / lower_50_cases_hosp,
    pub_hosp_u50 = upper_50_cases_publish / upper_50_cases_hosp,
    # cases_publish / deaths
    pub_deaths_med = median_cases_publish / median_deaths,
    pub_deaths_l90 = lower_90_cases_publish / lower_90_deaths,
    pub_deaths_u90 = upper_90_cases_publish / upper_90_deaths,
    pub_deaths_l50 = lower_50_cases_publish / lower_50_deaths,
    pub_deaths_u50 = upper_50_cases_publish / upper_50_deaths,
    # cases_hosp / deaths
    hosp_deaths_med = median_cases_hosp / median_deaths,
    hosp_deaths_l90 = lower_90_cases_hosp / lower_90_deaths,
    hosp_deaths_u90 = upper_90_cases_hosp / upper_90_deaths,
    hosp_deaths_l50 = lower_50_cases_hosp / lower_50_deaths,
    hosp_deaths_u50 = upper_50_cases_hosp / upper_50_deaths,
    # Drop duplicate date/regions with missing values for all (?)
    missing = ifelse(is.na(pub_hosp_med), TRUE, FALSE)) %>%
  dplyr::filter(missing == FALSE)

if(length(seq.Date(from = min(summary_wide$date), to = max(summary_wide$date), by = 1)) 
   != (length(summary_wide$date) / length(unique(summary_wide$region)))){
  warning("Missing or duplicate days in date sequence")
}

saveRDS(summary_wide, "rt-estimate/summary_wide.rds")
