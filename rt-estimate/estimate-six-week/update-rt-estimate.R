# Run Rt estimates for different data sources
# remotes::install_github("epiforecasts/covidregionaldata")
# remotes::install_github("epiforecasts/EpiNow2")
library(EpiNow2)

# Update delays
# source(here::here("rt-estimate", "delays", "public-linelist-delays.R"))

# Get delays and fresh UK data; set up cores
source(here::here("rt-estimate", "utils", "rt-data-defaults.R"))

# Get function for Rts
source(here::here("rt-estimate", "utils",  "run-rt-estimate.R"))

# Set min/max dates for data
# Min: 6 weeks + 6 days truncation; max: hospital admissions always 1 day more delayed than other data
data <- data[data$date >= Sys.Date()-(42+6) & data$date <= max(data$date)-1,] 

# Set root for saving estimates
save_loc <- "rt-estimate/estimate-six-week"

# Run estimates -----------------------------------------------------------
# Run everything everywhere
# Cases - by test date
run_rt_estimate(data = data,
                save_loc = save_loc,
                count_variable = c("cases_test"),
                reporting_delay = cases_delay,
                burn_in = 0,
                no_cores = no_cores,
                future = FALSE)
# Hospital admissions - first admissions only
run_rt_estimate(data = data,
                save_loc = save_loc,
                count_variable = c("cases_hosp_new"),
                reporting_delay = cases_delay,
                burn_in = 0,
                no_cores = no_cores,
                future = FALSE)
# Deaths - by date of death
run_rt_estimate(data = data,
                save_loc = save_loc,
                count_variable = c("deaths_death"),
                reporting_delay = deaths_delay,
                burn_in = 0,
                no_cores = no_cores,
                future = FALSE)



