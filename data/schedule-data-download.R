library(magrittr)
library(data.table)
library(ukcovid19) # remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")

# Get data ----------------------------------------------------------------

# Set up query
structure <- list("date", "areaName", 
                  "newDeaths28DaysByDeathDate",
                  "newCasesBySpecimenDate", 
                  "newAdmissions")
names(structure) <- structure

areaType <- list("nhsregion" = "areaType=nhsregion",
                 "region" = "areaType=region",
                 "nation" = "areaType=nation")

# Get data
raw <- purrr::map(areaType, ~ ukcovid19::get_data(filters = .x, structure = structure)) 

# Save
saveRDS(raw, paste0("data/", Sys.Date(),"-raw-uk-data.rds"))

