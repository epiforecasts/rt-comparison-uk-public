library(EpiNow2, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(here)
library(data.table)

# Set up running a single Rt forecast -------------------------------------
run_rt_breakpoint <- function(data, 
                            truncate = 5,
                            count_variable, 
                            reporting_delay,
                            save_loc,
                            no_cores,
                            max_execution_time = 60*60) {
  
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
    
    # Include breakpoint
    data_select <- data_select[, .(date, region, confirm, breakpoint)]
    
    # truncate 5 days
    data_select <- data_select[, .SD[date <= (Sys.Date() - lubridate::days(truncate))], by = region]
    
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
                           delays = list(incubation_period, reporting_delay),
                           horizon = 14, 
                           burn_in = 14, 
                           samples = 4000,
                           stan_args = list(warmup = 1000, 
                                            cores = no_cores, 
                                            chains = ifelse(no_cores <= 4, 4, no_cores)),
                           fixed_future_rt = TRUE,
                           target_folder = targets,
                           return_estimates = FALSE, 
                           summary = TRUE,
                           return_timings = TRUE, 
                           future = TRUE,
                           max_execution_time = max_execution_time,
                           breakpoint = TRUE)
    
    futile.logger::flog.debug("resetting future plan to sequential")
    future::plan("sequential")
    
    futile.logger::flog.trace("generating summary data")
    regional_summary(
      reported_cases = data_select, 
      results_dir = targets, 
      summary_dir =  summary, 
      return_summary = FALSE)
    
  }
}










