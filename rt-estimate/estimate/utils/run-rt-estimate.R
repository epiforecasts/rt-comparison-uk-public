library(EpiNow2, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(here)
library(data.table)

# Set up running a single Rt forecast -------------------------------------
run_rt_estimate <- function(data, count_variable, reporting_delay) {
  
  for(i in 1:length(count_variable)){
    
    print(paste0("Estimates for ", count_variable[i]))
    
  # Set up directories for models -------------------------------------------

  if(!dir.exists(here::here("rt-estimate", "estimate", 
                            count_variable[i]))) {
    dir.create(here::here("rt-estimate", "estimate", 
                          count_variable[i]))
  }
  
  targets <- paste0("rt-estimate/estimate/", count_variable[i], "/region")
  summary <- paste0("rt-estimate/estimate/", count_variable[i], "/summary")
  
  # Format for epinow2 ------------------------------------------------------

  data_select <- data.table::as.data.table(data)
  
  data_select <- data.table::setnames(data_select, old = count_variable[i], new = "confirm")
  
  data_select <- data_select[, .(date, region, confirm)]
  
  data_select <- data_select[, .SD[date <= (max(date) - lubridate::days(5))], by = region]
  
  data.table::setorder(data_select, date)
  
  # Check full date sequence
  if(length(data_select$date) != 
     (length(seq.Date(from = min(data_select$date), to = max(data_select$date), by = 1)) 
        * length(unique(data_select$region)))) {
    return(warning("Missing days in date sequence"))
  }
  
  
  # Set up common settings --------------------------------------------------
  
  EpiNow2::regional_epinow(target_folder = targets,
                           summary_dir = summary,
                           reported_cases = data_select,
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

  }
}


