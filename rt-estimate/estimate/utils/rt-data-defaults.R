# Packages -----------------------------------------------------------------
library(EpiNow2)
library(data.table)
library(future)

# Update delays -----------------------------------------------------------

generation_time <- readRDS(here::here("rt-estimate", "estimate", "delays", "data", "generation_time.rds"))
incubation_period <- readRDS(here::here("rt-estimate","estimate", "delays", "data", "incubation_period.rds"))

# Deaths delay
deaths_delay <- readRDS(here::here("rt-estimate", "estimate", "delays", "data", "cocin_onset_to_death_delay.rds"))

# Cases delay
cases_delay <- readRDS(here::here("rt-estimate","estimate", "delays", "data", "public_onset_to_report_delay.rds"))


# Get UK data ----------------------------------------------------------------
# If data won't download, read in a saved hard copy of cleaned data - 23 August 2020
if(class(try(source(here::here("data", "get-uk-data.R")))) == "try-error") {
  message("--- API failure - loading saved data ---")
  data <- readRDS("data/200922.rds")
} else {
  source(here::here("data", "get-uk-data.R"))
}


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

no_cores <- setup_future(length(unique(data$region)))