# For use with EpiNow2 v1.2 as used in the paper

library(EpiNow2, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(here)
library(data.table)

# Set up running a single Rt forecast -------------------------------------
run_rt_estimate <- function(data, 
                            save_loc,
                            count_variable, 
                            reporting_delay,
                            no_cores,
                            burn_in = 0,
                            future = TRUE,
                            max_execution_time = Inf) {
  
  for(i in 1:length(count_variable)){
    
    print(paste0("Estimates for ", count_variable[i]))
    
  # Set up directories for models -------------------------------------------

  if(!dir.exists(here::here(save_loc, 
                            count_variable[i]))) {
    dir.create(here::here(save_loc, 
                          count_variable[i]))
  }
  
  targets <- paste0(save_loc, count_variable[i], "/region")
  summary <- paste0(save_loc, count_variable[i], "/summary")
  
  # Format for epinow2 ------------------------------------------------------

  data_select <- data.table::as.data.table(data)
  
  data_select <- data.table::setnames(data_select, old = count_variable[i], new = "confirm")
  
  data_select <- data_select[, .(date, region, confirm)]
  
  data_select <- data_select[, .SD[date <= (Sys.Date() - lubridate::days(5))], by = region]
  
  data.table::setorder(data_select, date)

  # Set up log
  setup_log <- function(threshold = "INFO", file = "info.log") {
   futile.logger::flog.threshold(threshold)
    futile.logger::flog.appender(futile.logger::appender.tee(file))
    return(invisible(NULL))
  }
  
  
  # Set up common settings --------------------------------------------------
  
  futile.logger::flog.trace("calling regional_epinow")
  out <- regional_epinow(reported_cases = data_select,
                         generation_time = generation_time,
                         delays = list(incubation_period, 
                                       reporting_delay),
                         horizon = 0, 
                         burn_in = burn_in, 
                         samples = 4000,
                         stan_args = list(warmup = 1000, 
                                          cores = no_cores, 
                                          chains = ifelse(no_cores <= 4, 4, no_cores)),
                         target_folder = targets,
                         return_estimates = FALSE, 
                         summary = TRUE,
                         return_timings = TRUE, 
                         future = future,
                         max_execution_time = max_execution_time)
  
  futile.logger::flog.debug("resetting future plan to sequential")
  future::plan("sequential")
  
  futile.logger::flog.trace("generating summary data")
  regional_summary(
    reported_cases = data_select, 
    results_dir = targets, 
    summary_dir =  summary)

  }
}










