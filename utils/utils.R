# Plot colours
# Data: data source
colours <- c(
  # Test-positive cases
  "Test-positive" = "#1b9e77",
  "Cases" = "#1b9e77",
  "cases_test" =  "#1b9e77",
  # Admissions
  "Hospital admissions" =  "#7570b3",
  "cases_hosp" =  "#7570b3",
  # Deaths
  "Deaths" = "#d95f02",
  "deaths_death" = "#d95f02"
  )


# Order nations and regions
nation_order <- c("England")
nhsregion_order <- c("North East and Yorkshire" ,
                "North West",
                "Midlands",
                "East of England",
                "London",
                "South East",
                "South West")
region_names <- list(
  "nations" = factor(nation_order, levels = nation_order, labels = nation_order),
  "nhsregions" = factor(nhsregion_order, levels = nhsregion_order, labels = nhsregion_order),
  "region_factor" = factor(c(nation_order, nhsregion_order),
                           levels = c(nation_order, nhsregion_order))
  )

# saveRDS(region_names, "data/region_names.rds")
 

# Convert week to date
week_to_date <- function(df){
  seq_dates <- tibble::tibble(
    date = seq.Date(from = as.Date("2020-01-01"), length.out = 365, by = 1),
    days = weekdays(date),
    week = lubridate::epiweek(date)) %>%
    dplyr::filter(week %in% df$week & 
                    days == "Saturday") %>%
    dplyr::select(week, week_end_date = date)
  df_dates <- df %>%
    dplyr::left_join(seq_dates, by = "week")
  return(df_dates)
}