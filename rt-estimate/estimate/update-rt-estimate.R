# Run Rt estimates for different data sources
# remotes::install_github("epiforecasts/covidregionaldata")
# remotes::install_github("epiforecasts/EpiNow2")
library(EpiNow2)

# Update delays
source(here::here("rt-estimate", "estimate", "delays", "update-delays.R"))

# Get delays and fresh UK data; set up cores
source(here::here("rt-estimate", "estimate", "utils", "rt-data-defaults.R"))

# Get function for Rts
source(here::here("rt-estimate", "estimate", "utils",  "run-rt-estimate.R"))


# Run estimates -----------------------------------------------------------

start <- Sys.time()
# Cases - test date
run_rt_estimate(data = data[data$region == "England",],
                count_variable = c("cases_blend"), #  "cases_test", "cases_publish"),
                reporting_delay = cases_delay)
# Hospital admissions
# run_rt_estimate(data = data,
#                 count_variable = c("cases_hosp"), #  "cases_test", "cases_publish"),
#                 reporting_delay = cases_delay,
#                 burn_in = 11) # Hospital cases don't start until 11 days into the time-series for English regions (i.e. they start at eg 50 cases))
# Deaths - date of death
run_rt_estimate(data = data[data$region == "England",],
                count_variable = c("deaths_blend"), # "deaths_publish", "deaths_death"),
                reporting_delay = deaths_delay)

# 
# end <- Sys.time()
# runtime <- end - start
# saveRDS(runtime, "rt-estimate/three_var_runtime.rds")

