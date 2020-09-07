# Get UK data from which to estimate Rt
library(magrittr)
library(data.table)
library(ukcovid19) # remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")

# Include national data?
national_data = TRUE

# Get data ----------------------------------------------------------------

# Set up query
structure <- list("date", "areaName", 
                  "newDeaths28DaysByDeathDate",  "newDeaths28DaysByPublishDate",
                  "newCasesBySpecimenDate", "newCasesByPublishDate", "newAdmissions")
names(structure) <- structure

areaType <- list("nhsregion" = "areaType=nhsregion",
                 "region" = "areaType=region")
if(national_data){
  areaType$nation = "areaType=nation"
}



# Get data
raw <- purrr::map(areaType, ~ ukcovid19::get_data(filters = .x, structure = structure)) 


# Re-organise regions -----------------------------------------------------

# Take only admissions from NHS region data
setDT(raw$nhsregion)
raw$nhsregion <- raw$nhsregion[, .(date, areaName, newAdmissions)]

# Set Midlands to one region to match NHS regions (given that is where admissions data are available)
setDT(raw$region)
raw$midlands <- raw$region[areaName %in% c("East Midlands", "West Midlands") ][, by = date, .(newDeaths28DaysByDeathDate = sum(newDeaths28DaysByDeathDate),
                                                                                                newDeaths28DaysByPublishDate = sum(newDeaths28DaysByPublishDate),
                                                                                                newCasesBySpecimenDate = sum(newCasesBySpecimenDate),
                                                                                                newCasesByPublishDate = sum(newCasesByPublishDate),
                                                                                                newAdmissions = sum(newAdmissions),
                                                                                                areaName = "Midlands")]

# Set NE & Yorkshire to one region to match NHS regions
raw$ney <- raw$region[areaName %in% c("North East", "Yorkshire and The Humber") ][, by = date, .(newDeaths28DaysByDeathDate = sum(newDeaths28DaysByDeathDate),
                                                                                              newDeaths28DaysByPublishDate = sum(newDeaths28DaysByPublishDate),
                                                                                              newCasesBySpecimenDate = sum(newCasesBySpecimenDate),
                                                                                              newCasesByPublishDate = sum(newCasesByPublishDate),
                                                                                              newAdmissions = sum(newAdmissions),
                                                                                              areaName = "North East and Yorkshire")]

# Separate all other regions
raw$region_else <- raw$region[!areaName %in% c("East Midlands", "West Midlands", "North East", "Yorkshire and The Humber")]

# Bind Midlands, North East and Yorkshire, other regions
data <- rbind(raw$region_else, raw$midlands, raw$ney)

# Set to NA where data show 0 but are NA; remove admissions column before merge
data <- data[, c("newDeaths28DaysByPublishDate", "newCasesByPublishDate") := NA][, "newAdmissions" := NULL]

# Merge regional data with regional hospital admissions
data <- merge(data, raw$nhsregion, by = c("date", "areaName"))

# Bind regions with national
if(national_data) {
  data <- rbind(raw$nation, data)
}


# Add column to identify regions vs nations
source("utils/utils.R")
data$region_type <- ifelse(data$areaName %in% region_names$nation, "nation", "region")


# Cleaning ----------------------------------------------------------------

# Rename
data <- data.table::as.data.table(data)
old <- unlist(structure)
new <- c("date", "region", "deaths_death", "deaths_publish",  "cases_test", "cases_publish", "cases_hosp")
data <- data.table::setnames(data, old, new)

# Set date sequence to start from 1 Feb (arbitrary)
data$date <- lubridate::ymd(data$date)
data <- data[, .SD[date >= lubridate::ymd("2020-02-01")]]

# Add blended cases - publish date for nations, specimen date for regions
data$cases_blend = ifelse(data$region %in% region_names$nation, data$cases_publish, data$cases_test)

# Add blended deaths - publish date for nations, date of death for regions
data$deaths_blend = ifelse(data$region %in% region_names$nation, data$deaths_publish, data$deaths_death)

# Check date sequence is complete
if(length(seq.Date(from = min(data$date), to = max(data$date), by = 1)) 
   != (length(data$date) / length(unique(data$region)))){
  warning("Missing days in date sequence")
}


# Clean environment -------------------------------------------------------
#
rm(old, new, structure, areaType, raw)

