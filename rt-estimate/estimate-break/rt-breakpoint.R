library(EpiNow2, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(here)
library(data.table)

# Set up running a single Rt forecast -------------------------------------
run_rt_breakpoint <- function(data, 
                            truncate = 3,
                            count_variable, 
                            reporting_delay,
                            save_loc,
                            no_cores,
                            generation_time,
                            incubation_period) {
  
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
    
    # Set up common settings --------------------------------------------------
    # Website settings
    # out <- regional_epinow(reported_cases = data_select,
    #                        generation_time = generation_time,
    #                        delays = list(incubation_period, reporting_delay),
    #                        horizon = 14, 
    #                        burn_in = 14, 
    #                        samples = 4000,
    #                        stan_args = list(warmup = 1000, 
    #                                         cores = no_cores, 
    #                                         chains = ifelse(no_cores <= 4, 4, no_cores)),
    #                        target_folder = targets,
    #                        future = TRUE,
    #                        logs = "rt-estimate/logs",
    #                        summary_args = list(reported_cases = data_select, 
    #                                            results_dir = targets, 
    #                                            summary_dir =  summary),
    #                        verbose = TRUE)
    # 
    
    # US forecast settings
    regional_epinow( # standard settings (US forecast)
      samples = 2000, 
      horizon = 14, 
      generation_time = generation_time,
      delays = list(incubation_period, reporting_delay),
      stan_args = list(warmup = 500, 
                       cores = no_cores, 
                       control = list(adapt_delta = 0.95,
                                      max_treedepth = 15), 
                       chains = ifelse(no_cores <= 4, 4, no_cores)), 
      burn_in = 14, 
      non_zero_points = 2,
      max_execution_time = 60 * 30, 
      future = TRUE,
      output = c("region", "samples", "summary", "timing"),
      # Custom settings
      reported_cases = data_select,
      target_folder = targets,
      summary_args = list(summary_dir = summary,
                          all_regions = FALSE),
      logs = "rt-estimate/logs/breakpoint",
      future_rt = "latest")
    
  }
}
