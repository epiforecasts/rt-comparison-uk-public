# Run Rt estimates for different data sources
# remotes::install_github("epiforecasts/covidregionaldata")
# remotes::install_github("epiforecasts/EpiNow2")
library(EpiNow2)

# Update delays
# source(here::here("rt-estimate", "estimate", "delays", "public-linelist-delays.R"))

# Get delays and fresh UK data; set up cores
source(here::here("rt-estimate", "estimate", "utils", "rt-data-defaults.R"))

# Get function for Rts
source(here::here("rt-estimate", "estimate", "utils",  "run-rt-estimate.R"))

# Set min/max dates for data
data <- data[data$date >= as.Date("2020-03-19") & data$date <= Sys.Date()-2,] 
  # regional data only available after 19 March; this trims England data to same
  # hospital admissions always 1 day more delayed than other data

# Run estimates -----------------------------------------------------------
# Run everything everywhere
# 
# Cases - by test date
# run_rt_estimate(data = data,
#                 count_variable = c("cases_test"),
#                 reporting_delay = cases_delay,
#                 burn_in = 0)
# # Hospital admissions
# run_rt_estimate(data = data,
#                 count_variable = c("cases_hosp"), 
#                 reporting_delay = cases_delay,
#                 burn_in = 0)
# Deaths - by date of death
run_rt_estimate(data = data,
                count_variable = c("deaths_death"),
                reporting_delay = deaths_delay,
                burn_in = 0)



