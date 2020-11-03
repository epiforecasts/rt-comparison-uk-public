# Run breakpoint estimates

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


# get public data ----------------------------------------------------------------
# # # Public data
# # Set up query
# structure <- list("date", "areaName", 
#                   "newDeaths28DaysByDeathDate",
#                   "newCasesBySpecimenDate", 
#                   "newAdmissions")
# names(structure) <- structure
# areaType <- list("nation" = "areaType=nation")
# 
# # Get data
# raw <- ukcovid19::get_data(filters = areaType, structure = structure)
# # clean
# data <- data.table::as.data.table(raw)
# old <- unlist(structure)
# new <- c("date", "region", "deaths_death",  "cases_test", "cases_hosp")
# data <- data.table::setnames(data, old, new)


# get private data --------------------------------------------------------
raw <- readRDS("covid19_uk_forecast_data/data/processed/latest_data.rds")
raw$value_desc <- NULL
data <- raw[raw$type == "Data" ,]
data <- tidyr::pivot_wider(data, values_from = "value", names_from = "value_type")
data$type <- NULL
data <- data[,c("value_date", "geography", "death_inc_line", "hospital_inc", "reported_cases")]
colnames(data) <- c("date", "region", "deaths", "admissions", "cases")

data <- as.data.table(data)

# Set date sequence to start from 6wks
data$date <- lubridate::ymd(data$date)
data <- data[, .SD[date >= (max(date)-42)], by = region]

# Keep regions
data <- data[region %in% c("South West", "Wales", "Northern Ireland")]

# Add breakpoints
data <- data[, breakpoint := data.table::fifelse( (date == as.Date("2020-10-16") & 
                                                     region == "Northern Ireland") | 
                                                    (date == as.Date("2020-10-24") & 
                                                       region %in% c("Wales", "South West")), 
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


# breakpoint only ---------------------------------------------------------
# # Set root for saving estimates
save_loc <- "rt-estimate/estimate-break/breakpoint-only/"
# # Cases
cases <- run_rt_breakpoint(data = data,
                           type = "breakpoint",
                           truncate = 0,
                           count_variable = "cases",
                           reporting_delay = cases_delay,
                           generation_time = generation_time,
                           incubation_period = incubation_period,
                           save_loc = save_loc,
                           no_cores = no_cores)
# Admissions
adm <- run_rt_breakpoint(data = data,
                         type = "breakpoint",
                         truncate = 0,
                         count_variable = "admissions",
                         reporting_delay = cases_delay,
                         generation_time = generation_time,
                         incubation_period = incubation_period,
                         save_loc = save_loc,
                         no_cores = no_cores)
# Deaths
deaths <- run_rt_breakpoint(data = data,
                            type = "breakpoint",
                            truncate = 0,
                            count_variable = "deaths",
                            reporting_delay = deaths_delay,
                            generation_time = generation_time,
                            incubation_period = incubation_period,
                            save_loc = save_loc,
                            no_cores = no_cores)



# With RW -----------------------------------------------------------------

# # Add multiple breakpoints, weekly on Sundays, for random walk
sundays <- data[weekdays(data$date)=="Sunday", "date"]

break_ni <- as.Date("2020-10-16")
break_ni <- c(sundays[date <= (break_ni - 6)]$date, break_ni)

break_wales <- as.Date("2020-10-24")
break_wales <- c(sundays[date <= (break_wales - 6)]$date, break_wales)

data_breaks <- data[, breakpoint := data.table::fifelse((date %in% break_ni &
                                                           region == "Northern Ireland") |
                                                          (date %in% break_wales &
                                                             region %in%
                                                             c("Wales", "South West")),
                                                        1, 0)]

#Run Rt
save_loc <- "rt-estimate/estimate-break/breakpoint-with-rw/"

# Cases
cases <- run_rt_breakpoint(data = data_breaks,
                           type = "breakpoint",
                           truncate = 0,
                           count_variable = "cases",
                           reporting_delay = cases_delay,
                           generation_time = generation_time,
                           incubation_period = incubation_period,
                           save_loc = save_loc,
                           no_cores = no_cores)
# Admissions
adm <- run_rt_breakpoint(data = data_breaks,
                         type = "breakpoint",
                         truncate = 0,
                         count_variable = "admissions",
                         reporting_delay = cases_delay,
                         generation_time = generation_time,
                         incubation_period = incubation_period,
                         save_loc = save_loc,
                         no_cores = no_cores)
# Deaths
deaths <- run_rt_breakpoint(data = data_breaks,
                            type = "breakpoint",
                            truncate = 0,
                            count_variable = "deaths",
                            reporting_delay = deaths_delay,
                            generation_time = generation_time,
                            incubation_period = incubation_period,
                            save_loc = save_loc,
                            no_cores = no_cores)


# GP + 1 breakpoint ---------------------------------------------------------
# Set root for saving estimates
save_loc <- "rt-estimate/estimate-break/gp-only/"
cases <- run_rt_breakpoint(data = data,
                           type = "gp",
                           truncate = 0,
                            count_variable = "cases",
                           reporting_delay = cases_delay,
                           generation_time = generation_time,
                           incubation_period = incubation_period,
                           save_loc = save_loc,
                           no_cores = no_cores)
#Admissions
adm <- run_rt_breakpoint(data = data,
                         type = "gp",
                         truncate = 0,
                         count_variable = "admissions",
                         reporting_delay = cases_delay,
                         generation_time = generation_time,
                         incubation_period = incubation_period,
                         save_loc = save_loc,
                         no_cores = no_cores)
# Deaths
deaths <- run_rt_breakpoint(data = data,
                  type = "gp",
                  truncate = 0,
                  count_variable = "deaths",
                  reporting_delay = deaths_delay,
                  generation_time = generation_time,
                  incubation_period = incubation_period,
                  save_loc = save_loc,
                  no_cores = no_cores)
