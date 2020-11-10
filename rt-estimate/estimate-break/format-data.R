# Format data
# raw <- readRDS(path.expand(file.path("~", "code", "covid19_uk_forecast_data", "data", "processed", "latest_data.rds")))
raw <- readRDS(path.expand(file.path("C:", "Users", "kaths", "Github", "covid19_uk_forecast_data", "data", "processed", "latest_data.rds")))

##
raw$value_desc <- NULL
data <- raw[raw$type == "Data" ,]
data <- tidyr::pivot_wider(data, values_from = "value", names_from = "value_type")
data$type <- NULL
data <- data[,c("value_date", "geography", "death_inc_line", "hospital_inc", "reported_cases")]
colnames(data) <- c("date", "region", "deaths", "admissions", "cases")
data <- as.data.table(data)
data$date <- lubridate::ymd(data$date)
data <- data[, .SD[date >= (max(date)-42)], by = region]
data <- data[region %in% c("Wales", "Northern Ireland", "South West")]
data <- data[, breakpoint := data.table::fifelse( (date == as.Date("2020-10-16") & 
                                                     region == "Northern Ireland") | 
                                                    (date == as.Date("2020-10-24") & 
                                                       region == "Wales"), 
                                                  1, 0)]
# moving average
data_ma <- data %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(
    # 7-day moving average
    cases = forecast::ma(cases, order = 7), 
    admissions = forecast::ma(admissions, order = 7), 
    deaths = forecast::ma(deaths, order = 7)) %>% 
  dplyr::ungroup() %>%
  tidyr::pivot_longer(-c(region, date, breakpoint), names_to = "source", values_to = "ma")

data <- data %>%
  tidyr::pivot_longer(-c(region, date, breakpoint), names_to = "source", values_to = "value")

# Save
data_list <- list("data" = data,
             "data_ma" = data_ma)

saveRDS(data_list, here::here("rt-estimate", "estimate-break", Sys.Date(), 
                   "formatted-data.rds"))
