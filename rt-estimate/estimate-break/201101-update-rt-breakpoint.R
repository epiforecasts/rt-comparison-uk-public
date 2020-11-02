# Packages -----------------------------------------------------------------
library(EpiNow2)
library(data.table)
library(future)
# install.packages("EpiNow2")

# Delays -----------------------------------------------------------
# Update
# source(here::here("rt-estimate", "delays", "public-linelist-delays.R"))

# Fixed
generation_time <- readRDS(here::here("rt-estimate", "delays", "data", "generation_time.rds"))
incubation_period <- readRDS(here::here("rt-estimate", "delays", "data", "incubation_period.rds"))

# Deaths delay
deaths_delay <- readRDS(here::here("rt-estimate", "delays", "data", "cocin_onset_to_death_delay.rds"))

# Cases delay
cases_delay <- readRDS(here::here("rt-estimate", "delays", "data", "public_onset_to_report_delay.rds"))


# get data ----------------------------------------------------------------
# Set up query
structure <- list("date", "areaName", 
                  "newDeaths28DaysByDeathDate",
                  "newCasesBySpecimenDate", 
                  "newAdmissions")
names(structure) <- structure
areaType <- list("nation" = "areaType=nation")

# Get data
raw <- ukcovid19::get_data(filters = areaType, structure = structure)

# clean
data <- data.table::as.data.table(raw)
old <- unlist(structure)
new <- c("date", "region", "deaths_death",  "cases_test", "cases_hosp")
data <- data.table::setnames(data, old, new)

# Set date sequence to start from 12wks
data$date <- lubridate::ymd(data$date)
data <- data[, .SD[date >= max(date)-84]]

# Remove England/Scotland
data <- data[region %in% c("Wales", "Northern Ireland")]

# Add breakpoints
data <- data[, breakpoint := data.table::fifelse( (date == as.Date("2020-10-16") & 
                                                   region == "Northern Ireland") | 
                                                   (date == as.Date("2020-10-24") & 
                                                   region == "Wales"), 
                                                  1, 0)]

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

# Rt estimate -------------------------------------------------------------

# Get function for Rts
source(here::here("rt-estimate", "estimate-break",  "rt-breakpoint.R"))

# Set root for saving estimates
save_loc <- "rt-estimate/estimate-break/"

# Cases
run_rt_breakpoint(data = data, 
                truncate = 3,
                count_variable = "cases_test", 
                reporting_delay = cases_delay,
                generation_time = generation_time,
                incubation_period = incubation_period,
                save_loc = save_loc,
                no_cores = no_cores) 
#Admissions
run_rt_breakpoint(data = data, 
                truncate = 3,
                count_variable = "cases_hosp", 
                reporting_delay = cases_delay,
                generation_time = generation_time,
                incubation_period = incubation_period,
                save_loc = save_loc,
                no_cores = no_cores) 
# Deaths
run_rt_breakpoint(data = data, 
                truncate = 3,
                count_variable = "deaths_death", 
                reporting_delay = deaths_delay,
                generation_time = generation_time,
                incubation_period = incubation_period,
                save_loc = save_loc,
                no_cores = no_cores) 













