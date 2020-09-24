# Packages ----------------------------------------------------------------

require(EpiNow2)
require(covidregionaldata)
require(data.table)
require(future)
require(lubridate)

# Save incubation period and generation time ------------------------------

generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                        mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                        sd = EpiNow2::covid_generation_times[1, ]$sd,
                        sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                        max = 30)

incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
                          mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
                          sd = EpiNow2::covid_incubation_period[1, ]$sd,
                          sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
                          max = 30)


saveRDS(generation_time , here::here("rt-estimate", "estimate", "delays", "data", "generation_time.rds"))
saveRDS(incubation_period, here::here("rt-estimate", "estimate", "delays", "data", "incubation_period.rds"))

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

report_delay <- data.table::as.data.table(report_delay)[!(country %in% c("Mexico", "Phillipines"))]

public_onset_to_report_delay <- EpiNow2::bootstrapped_dist_fit(report_delay$days_onset_to_report, bootstraps = 100, 
                                                           bootstrap_samples = 250)

## Set max allowed delay to 30 days to truncate computation
public_onset_to_report_delay$max <- 30

saveRDS(public_onset_to_report_delay, here::here("rt-estimate", "estimate", "delays", "data", "public_onset_to_report_delay.rds"))

# Fit delay from onset to deaths ------------------------------------------
# Simulate Poisson distribution around 20 days delay onset to death
# Source: 
#   UK ONS 
#   https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/bulletins/coronaviruscovid19relateddeathsbyoccupationbeforeandduringlockdownenglandandwales/deathsregisteredbetween9marchand30jun2020

sim_delay_death <- rpois(1000, lambda = 20)

uk_sim_onset_to_death_delay <- EpiNow2::bootstrapped_dist_fit(sim_delay_death, bootstraps = 100, bootstrap_samples = 250)

## Set max allowed delay to 30 days to truncate computation
uk_sim_onset_to_death_delay$max <- 30

saveRDS(uk_sim_onset_to_death_delay, here::here("rt-estimate", "estimate", "delays", "data", "uk_sim_onset_to_death_delay.rds"))



