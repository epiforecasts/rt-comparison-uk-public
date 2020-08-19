# Run Rt estimates for different data sources

# Update delays
source(here::here("rt-estimate", "estimate", "delays", "update-delays.R"))

# Get delays and fresh UK data; set up cores
source(here::here("rt-estimate", "estimate", "utils", "rt-data-defaults.R"))

# Get function for Rts
source(here::here("rt-estimate", "estimate", "utils",  "run-rt-estimate.R"))


# Run estimates -----------------------------------------------------------

# List by name and type of delay required
data_sources <- list("cases_test" = "cases_delay", 
                     "cases_publish" = "cases_delay", 
                     "cases_hosp" = "cases_delay",
                     "deaths" = "deaths_delay")

# Run all rt estimates
purrr::walk(data_sources, ~ run_rt_estimate, data = data, 
                                             count_variable = names(.x),
                                             reporting_delay = .x)

