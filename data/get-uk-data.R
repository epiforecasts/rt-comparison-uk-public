# Get UK data from which to estimate Rt
library(magrittr)
library(data.table)
library(ukcovid19) # remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")

# Set up query
structure <- list("date", "areaName", 
                  "newDeaths28DaysByDeathDate",  "newDeaths28DaysByPublishDate",
                  "newCasesBySpecimenDate", "newCasesByPublishDate", "newAdmissions")
names(structure) <- structure
areaType <- list("nhsregion" = "areaType=nhsregion",
                 "region" = "areaType=region",
                 "nation" = "areaType=nation")

# Get data
raw <- purrr::map(areaType, ~ ukcovid19::get_data(filters = .x, structure = structure)) 

# # Clean

# Set Midlands to one region to match NHS regions (given that is where admissions data are available)
setDT(raw$nhsregion)
raw$nhsregion <- raw$nhsregion[, .(date, areaName, newAdmissions)]

setDT(raw$region)
raw$midlands <- raw$region[areaName %in% c("East Midlands", "West Midlands") ][, by = date, .(newDeaths28DaysByDeathDate = sum(newDeaths28DaysByDeathDate),
                                                                                                newDeaths28DaysByPublishDate = NA,
                                                                                                newCasesBySpecimenDate = sum(newCasesBySpecimenDate),
                                                                                                newCasesByPublishDate = sum(newCasesByPublishDate),
                                                                                                newAdmissions = NA,
                                                                                                areaName = "Midlands")]

# Set NE & Yorkshire to one region to match NHS regions
setDT(raw$nhsregion)
raw$nhsregion <- raw$nhsregion[, .(date, areaName, newAdmissions)]

setDT(raw$region)
raw$ney <- raw$region[areaName %in% c("North East", "Yorkshire and The Humber") ][, by = date, .(newDeaths28DaysByDeathDate = sum(newDeaths28DaysByDeathDate),
                                                                                              newDeaths28DaysByPublishDate = NA,
                                                                                              newCasesBySpecimenDate = sum(newCasesBySpecimenDate),
                                                                                              newCasesByPublishDate = NA,
                                                                                              newAdmissions = NA,
                                                                                              areaName = "North East and Yorkshire")]

# Separate all other regions
raw$region_else <- raw$region[!areaName %in% c("East Midlands", "West Midlands", "North East", "Yorkshire and The Humber")]

# Bind Midlands, North East and Yorkshire, other regions, national
data <- rbind(raw$region_else, raw$midlands, raw$ney, raw$nation)

# Merge with hospital admissions
data <- merge(data, raw$nhsregion, by = c("date", "areaName"), all.x = TRUE)

# Keep existing admissions data at nation level
nations <- c("England", "Scotland", "Wales", "Northern Ireland")
nhsregions <- setdiff(data$region, nations)
region_names <- list("nation" = nations, "nhsregion" = nhsregions)
# saveRDS(region_names, "data/region_names.rds")

data$newAdmissions <- ifelse(data$areaName %in% region_names$nation, 
                             data$newAdmissions.x, data$newAdmissions.y)
data$newAdmissions.x <- NULL
data$newAdmissions.y <- NULL


# Rename
data.table::setDT(data)
old <- unlist(structure)
new <- c("date", "region", "deaths_death", "deaths_publish",  "cases_test", "cases_publish", "cases_hosp")
data <- data.table::setnames(data, old, new)
data$date <- as.Date(data$date)

# Add column to identify regions vs nations
data$region_type <- ifelse(data$region %in% region_names$nation, "nation", "region")

# Add blended cases - publish date for nations, specimen date for regions
data$cases_blend = ifelse(data$region %in% region_names$nation, data$cases_publish, data$cases_test)

# Add blended deaths - publish date for nations, date of death for regions
data$deaths_blend = ifelse(data$region %in% region_names$nation, data$deaths_publish, data$deaths_death)

# Regions show 0s where there should be NAs for data by publish date
data$cases_publish <- ifelse(data$region %in% region_names$nhsregion, NA, data$cases_publish)
data$deaths_publish <- ifelse(data$region %in% region_names$nhsregion, NA, data$deaths_publish)


# Check date sequence is complete
if(length(seq.Date(from = min(data$date), to = max(data$date), by = 1)) 
   != (length(data$date) / length(unique(data$region)))){
  warning("Missing days in date sequence")
}

rm(old, new, structure, areaType, raw, nations, nhsregions)



