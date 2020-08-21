# Get UK data from which to estimate Rt
library(magrittr)
library(data.table)
library(ukcovid19) # remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")

# Set up query
structure <- list("date", "areaName", 
                  "newDeaths28DaysByDeathDate", "newCasesBySpecimenDate", 
                  "newCasesByPublishDate", "newAdmissions")
names(structure) <- structure
areaType <- list("region" = "areaType=region",
                 "nation" = "areaType=nation")

# Get data
data <- purrr::map_dfr(areaType, ~ ukcovid19::get_data(filters = .x, structure = structure))

# # Clean

# Rename
data.table::setDT(data)
old <- unlist(structure)
new <- c("date", "region", "deaths", "cases_test", "cases_publish", "cases_hosp")
data <- data.table::setnames(data, old, new)
data$date <- as.Date(data$date)

# Add column to identify regions vs nations
nations <- c("England", "Scotland", "Wales", "Northern Ireland")
regions <- setdiff(unique(data$region), nations)
data$region_type <- ifelse(data$region %in% nations, "nation", "region")

# cases_publish - regions show 0s where there should be NAs for cases by publish date
data$cases_publish <- ifelse(data$region %in% regions, NA, data$cases_publish)

# Check date sequence is complete
if(length(seq.Date(from = min(data$date), to = max(data$date), by = 1)) 
   != (length(data$date) / length(unique(data$region)))){
  warning("Missing days in date sequence")
}

rm(old, new, structure, areaType)



