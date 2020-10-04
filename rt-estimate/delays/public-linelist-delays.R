# Packages ----------------------------------------------------------------

require(EpiNow2)
require(covidregionaldata)
require(data.table)
require(future)
require(lubridate)

# Save incubation period and generation time ------------------------------

generation_time <- EpiNow2::get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- EpiNow2::get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

saveRDS(generation_time , here::here("rt-estimate", "delays", "data", "generation_time.rds"))
saveRDS(incubation_period, here::here("rt-estimate", "delays", "data", "incubation_period.rds"))

# Set up parallel ---------------------------------------------------------

if (!interactive()) {
  ## If running as a script enable this
  options(future.fork.enable = TRUE)
}


plan(multiprocess)

# Fit delay from onset to report ---------------------------------------
# Source: 
#   International public linelist
#   Outliers removed (Mexico, Philippines)

report_delay <- covidregionaldata::get_linelist(report_delay_only = TRUE)

report_delay <- data.table::as.data.table(report_delay)[!(country %in% c("Mexico", "Philippines"))]

public_onset_to_report_delay <- EpiNow2::bootstrapped_dist_fit(report_delay$days_onset_to_report, 
                                                               bootstraps = 100, 
                                                               bootstrap_samples = 250,
                                                               max_value = 30)

## Set max allowed delay to 30 days to truncate computation
public_onset_to_report_delay$max <- 30

saveRDS(public_onset_to_report_delay, here::here("rt-estimate", "delays", "data", "public_onset_to_report_delay.rds"))

# Fit delay from onset to deaths ------------------------------------------

# Confidential CO-CIN onset to death distribution: run locally

# delay_death <- readRDS("onset_to_death.rds")
# cocin_onset_to_death_delay <- EpiNow2::bootstrapped_dist_fit(delay_death, 
#                                                              bootstraps = 100, 
#                                                              bootstrap_samples = 250,
#                                                              max_value = 30)
# 
# saveRDS(cocin_onset_to_death_delay, here::here("rt-estimate", "estimate", "delays", "data", "cocin_onset_to_death_delay.rds"))

# Exponentiate from lognormal distribution
# purrr::map2_dbl(cocin_onset_to_death_delay$mean, cocin_onset_to_death_delay$sd, ~ exp(.x + .y^2/2))

