# Packages ----------------------------------------------------------------

require(data.table, quietly = TRUE)
require(EpiNow2, quietly = TRUE)
require(dplyr)
require(tidyr)
require(purrr)

# Get national -------------------------------------------------------------

estimates <- list("cases_test",
                  "cases_publish",
                  "cases_hosp",
                  "deaths_publish",
                  "deaths_death")
names(estimates) <- estimates

# Get Rt only
summary <- estimates %>%
  purrr::map(~ readr::read_csv(paste0(here::here("rt-estimate", "estimate"), "/", ., "/summary/rt.csv"))) %>%
  purrr::map_dfr(~ .x, .id = "source") %>%
  # Filter out "estimate based on partial data"
  dplyr::filter(type == "estimate")

saveRDS(summary, "rt-estimate/summary.rds")

# Save max date for use in plotting
max_date <- max(summary$date)
saveRDS(max_date, file = "rt-estimate/max_data_date.rds")

# Identify regions vs nations
nations <- c("England", "Scotland", "Wales", "Northern Ireland")
all_regions <- list("nation" = factor(nations, levels = c("England", "Scotland", "Wales", "Northern Ireland")), 
                    "region" = setdiff(unique(summary$region), nations))
saveRDS(all_regions, "data/region_names.rds")

# Take ratios -------------------------------------------------------------
summary_wide <- summary %>%
  tidyr::pivot_wider(names_from = source, 
                     values_from = c("median","lower_90", "upper_90", 
                                     "lower_50", "upper_50"))  %>%
  dplyr::mutate(region_type = ifelse(region %in% nations, "nation", "region"),
                # Cases blend
                median_cases_blend = ifelse(region %in% nations, median_cases_publish, median_cases_test),
                lower_90_cases_blend = ifelse(region %in% nations, lower_90_cases_publish, lower_90_cases_test),
                upper_90_cases_blend = ifelse(region %in% nations, upper_90_cases_publish, upper_90_cases_test),
                lower_50_cases_blend = ifelse(region %in% nations, lower_50_cases_publish, lower_50_cases_test),
                upper_50_cases_blend = ifelse(region %in% nations, upper_50_cases_publish, upper_50_cases_test),
                # Deaths blend
                median_deaths_blend = ifelse(region %in% nations, median_deaths_publish, median_deaths_death),
                lower_90_deaths_blend = ifelse(region %in% nations, lower_90_deaths_publish, lower_90_deaths_death),
                upper_90_deaths_blend = ifelse(region %in% nations, upper_90_deaths_publish, upper_90_deaths_death),
                lower_50_deaths_blend = ifelse(region %in% nations, lower_50_deaths_publish, lower_50_deaths_death),
                upper_50_deaths_blend = ifelse(region %in% nations, upper_50_deaths_publish, upper_50_deaths_death)) %>%
  dplyr::mutate(
    # cases_blend / deaths
    caseb_deathb_med = median_cases_blend / median_deaths_blend,
    caseb_deathb_l90 = lower_90_cases_blend / lower_90_deaths_blend,
    caseb_deathb_u90 = upper_90_cases_blend / upper_90_deaths_blend,
    caseb_deathb_l50 = lower_50_cases_blend / lower_50_deaths_blend,
    caseb_deathb_u50 = upper_50_cases_blend / upper_50_deaths_blend,
    # cases_blend / cases_hosp
    caseb_hosp_med = median_cases_blend / median_cases_hosp,
    caseb_hosp_l90 = lower_90_cases_blend / lower_90_cases_hosp,
    caseb_hosp_u90 = upper_90_cases_blend / upper_90_cases_hosp,
    caseb_hosp_l50 = lower_50_cases_blend / lower_50_cases_hosp,
    caseb_hosp_u50 = upper_50_cases_blend / upper_50_cases_hosp,
    # cases_hosp / deaths_blend
    hosp_deathb_med = median_cases_hosp / median_deaths_blend,
    hosp_deathb_l90 = lower_90_cases_hosp / lower_90_deaths_blend,
    hosp_deathb_u90 = upper_90_cases_hosp / upper_90_deaths_blend,
    hosp_deathb_l50 = lower_50_cases_hosp / lower_50_deaths_blend,
    hosp_deathb_u50 = upper_50_cases_hosp / upper_50_deaths_blend)

if(length(seq.Date(from = min(summary_wide$date), to = max(summary_wide$date), by = 1)) 
   != (length(summary_wide$date) / length(unique(summary_wide$region)))){
  warning("Missing or duplicate days in date sequence")
}

saveRDS(summary_wide, "rt-estimate/summary_wide.rds")
