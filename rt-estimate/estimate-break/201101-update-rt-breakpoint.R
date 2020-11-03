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
raw <- readRDS("C:/Users/kaths/confidential-data/latest_data.rds")
raw$value_desc <- NULL
data <- raw[raw$type == "Data" ,]
data <- tidyr::pivot_wider(data, values_from = "value", names_from = "value_type")
data$type <- NULL
data <- data[,c("value_date", "geography", "death_inc_line", "hospital_inc", "reported_cases")]
colnames(data) <- c("date", "region", "deaths", "admissions", "cases")

data <- as.data.table(data)

# Set date sequence to start from 12wks
data$date <- lubridate::ymd(data$date)
data <- data[, .SD[date >= (max(date)-84)], by = region]

# Keep regions
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


# breakpoint only ---------------------------------------------------------
# Set root for saving estimates
# save_loc <- "rt-estimate/estimate-break/breakpoint-only/"
# # Cases
# cases <- run_rt_breakpoint(data = data,
#                            type = "breakpoint",
#                 truncate = 0,
#                 count_variable = "cases",
#                 reporting_delay = cases_delay,
#                 generation_time = generation_time,
#                 incubation_period = incubation_period,
#                 save_loc = save_loc,
#                 no_cores = no_cores)
# # Admissions
# adm <- run_rt_breakpoint(data = data, 
#                          type = "breakpoint",
#                 truncate = 0,
#                 count_variable = "admissions", 
#                 reporting_delay = cases_delay,
#                 generation_time = generation_time,
#                 incubation_period = incubation_period,
#                 save_loc = save_loc,
#                 no_cores = no_cores) 
# # Deaths
# deaths <- run_rt_breakpoint(data = data, 
#                   type = "breakpoint",
#                 truncate = 0,
#                 count_variable = "deaths", 
#                 reporting_delay = deaths_delay,
#                 generation_time = generation_time,
#                 incubation_period = incubation_period,
#                 save_loc = save_loc,
#                 no_cores = no_cores) 



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
                                                             region == "Wales"), 
                                                        1, 0)]

# Run Rt
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
# save_loc <- "rt-estimate/estimate-break/gp-breakpoint"
# cases <- run_rt_breakpoint(data = data,
#                            type = "gp-breakpoint",
#                            truncate = 3,
#                            count_variable = "cases_test",
#                            reporting_delay = cases_delay,
#                            generation_time = generation_time,
#                            incubation_period = incubation_period,
#                            save_loc = save_loc,
#                            no_cores = no_cores)
# #Admissions
# adm <- run_rt_breakpoint(data = data,
#                          type = "gp-breakpoint",
#                          truncate = 3,
#                          count_variable = "cases_hosp",
#                          reporting_delay = cases_delay,
#                          generation_time = generation_time,
#                          incubation_period = incubation_period,
#                          save_loc = save_loc,
#                          no_cores = no_cores)
# # Deaths
# deaths <- run_rt_breakpoint(data = data,
#                   type = "gp-breakpoint",
#                   truncate = 3,
#                   count_variable = "deaths_death",
#                   reporting_delay = deaths_delay,
#                   generation_time = generation_time,
#                   incubation_period = incubation_period,
#                   save_loc = save_loc,
#                   no_cores = no_cores)



# Format estimates --------------------------------------------------------
library(magrittr)
vars <- c("cases", "admissions", "deaths")
models <- c("breakpoint-only")
            #"breakpoint-with-rw",
            #"gp-breakpoint")
  
# Read in          
breakpoint_only <- purrr::map(vars, 
                           ~ readr::read_csv(
                            paste0("rt-estimate/estimate-break/", models[1], "/", .x, "/summary/rt.csv")))
names(breakpoint_only) <- c("cases", "admissions", "deaths")
breakpoint_only <- dplyr::bind_rows(breakpoint_only, .id = "source") %>%
  dplyr::mutate(model = "single breakpoint")

# Join all & clean
models <- dplyr::bind_rows(breakpoint_only) %>%
    dplyr::select(-strat)


# Plot utils --------------------------------------------------------------
colours <- c(
  "cases" = "#1b9e77",
  # Admissions
  "admissions" =  "#7570b3",
  # Deaths
  "deaths" = "#d95f02")

# Plot data --------------------------------------------------------------------

data_ma <- data %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(
    # 7-day moving average
    cases = forecast::ma(cases, order = 7), 
    admissions = forecast::ma(admissions, order = 7), 
    deaths = forecast::ma(deaths, order = 7)) %>% 
  dplyr::ungroup() %>%
  tidyr::pivot_longer(-c(region, date, breakpoint), names_to = "source", values_to = "ma")

# Plot function
plot_data_fn <- function(region_name, breakpoint_date = NA){
  data_ma %>%
    dplyr::filter(region %in% region_name) %>%
    ggplot() +
    geom_line(aes(x = date, y = log(as.numeric(ma)), 
                  colour = source)) +
    geom_vline(xintercept = as.Date(breakpoint_date), 
               lty = 3, colour = "grey_50") +
    cowplot::theme_cowplot() +
    # coord_cartesian(xlim = c(date_min, date_max)) +
    scale_color_manual(values = colours) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b") +
    theme(panel.spacing.x = unit(0.1, "cm"),
          panel.spacing.y = unit(0.1, "cm")) +
    guides(colour = FALSE) +
    labs(y = "Log 7-day MA", x = NULL, subtitle = region_name)
}


# Plot Rt + data ----------------------------------------------------------
plot_rt_fn <- function(region_name, breakpoint_date = NA){
  models %>%
    dplyr::filter(region %in% region_name) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, col = source, fill = source)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_50, ymax = upper_50),
                         alpha = 0.2, size = 0, colour = NA) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_90, ymax = upper_90),
                         alpha = 0.1, colour = NA) +
    ggplot2::geom_line(ggplot2::aes(y = median),
                       alpha = 0.9, size = 1) +
    geom_vline(xintercept = as.Date(breakpoint_date), 
               lty = 3, colour = "grey_50") +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::scale_color_manual(values = colours) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    #scale_y_continuous(breaks=seq(0, 1.4, by = 0.2)) +
    cowplot::theme_cowplot() +
    ggplot2::labs(y = "R", x = NULL, subtitle = region_name) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
}

plot_rt_fn("Wales")


