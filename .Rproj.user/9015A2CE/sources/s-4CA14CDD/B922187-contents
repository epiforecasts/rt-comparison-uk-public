require(EpiNow2, quietly = TRUE)
require(lubridate, quietly = TRUE)

# Set up running a single Rt forecast -------------------------------------
run_rt_estimate <- function(data, count_variable, reporting_delay) {
  
  # Set up directories for models -------------------------------------------

  if(!dir.exists(here::here("rt-estimate", "estimate", 
                            count_variable))) {
    dir.create(here::here("rt-estimate", "estimate", 
                          count_variable))
  }
  
  
  targets <- paste0("rt-estimate/estimate/", count_variable, "/region")
  summary <- paste0("rt-estimate/estimate/", count_variable, "/summary")
  
  # Format for epinow2 ------------------------------------------------------

  data <- setDT(data)
  
  data <- data.table::setnames(data, old = count_variable, new = "confirm")
  
  data <- data[, .(date, region, confirm)]
  
  data <- data[, .SD[date >= (max(date) - lubridate::weeks(12))], by = region]
  
  data.table::setorder(data, date)
  
  # Check full date sequence
  if(length(data$date) != 
     (length(seq.Date(from = min(data$date), to = max(data$date), by = 1)) 
        * length(unique(data$region)))) {
    return(warning("Missing days in date sequence"))
  }
  
  
  # Set up common settings --------------------------------------------------
  
  start <- Sys.time()
  EpiNow2::regional_epinow(target_folder = targets,
                           summary_dir = summary,
                           reported_cases = data,
                           delays = list(incubation_period, reporting_delay),
                           generation_time = generation_time,
                           horizon = 0,
                           samples = 2000,
                           warmup = 500,
                           burn_in = 14,
                           adapt_delta = 0.98,
                           cores = no_cores,
                           chains = ifelse(no_cores <= 2, 2, no_cores),
                           return_estimates = FALSE, verbose = TRUE)
  end <- Sys.time()

  }

