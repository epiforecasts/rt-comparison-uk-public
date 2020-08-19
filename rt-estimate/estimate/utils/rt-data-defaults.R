# Packages -----------------------------------------------------------------
require(EpiNow2)
require(data.table)
require(future)

# Update delays -----------------------------------------------------------

generation_time <- readRDS(here::here("rt-estimate", "estimate", "delays", "data", "generation_time.rds"))
incubation_period <- readRDS(here::here("rt-estimate","estimate", "delays", "data", "incubation_period.rds"))

# Deaths delay
deaths_delay <- readRDS(here::here("rt-estimate","estimate", "delays", "data", "onset_to_death_delay.rds"))

# Cases delay
cases_delay <- readRDS(here::here("rt-estimate","estimate", "delays", "data", "onset_to_admission_delay.rds"))

# Get UK data ----------------------------------------------------------------
source(here::here("data", "get-uk-data.R"))

# # # Set up cores -----------------------------------------------------
setup_future <- function(jobs) {
  if (!interactive()) {
    ## If running as a script enable this
    options(future.fork.enable = TRUE)
  }
  
  
  plan(tweak(multiprocess, workers = min(future::availableCores(), jobs)),
       gc = TRUE, earlySignal = TRUE)
  
  
  jobs <- max(1, round(future::availableCores() / jobs, 0))
  return(jobs)
}

no_cores <- setup_future(length(unique(deaths$region)))
