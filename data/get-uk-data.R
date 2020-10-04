# Get UK data from which to estimate Rt
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


# Re-organise regions -----------------------------------------------------

# Take only admissions from NHS region data
setDT(raw$nhsregion)
raw$nhsregion <- raw$nhsregion[, .(date, areaName, newAdmissions)]

# Set Midlands to one region to match NHS regions (given that is where admissions data are available)
setDT(raw$region)
raw$midlands <- raw$region[areaName %in% c("East Midlands", "West Midlands") ][, by = date, .(newDeaths28DaysByDeathDate = sum(newDeaths28DaysByDeathDate),
                                                                                                newCasesBySpecimenDate = sum(newCasesBySpecimenDate),
                                                                                                newAdmissions = sum(newAdmissions),
                                                                                                areaName = "Midlands")]

# Set NE & Yorkshire to one region to match NHS regions
raw$ney <- raw$region[areaName %in% c("North East", "Yorkshire and The Humber") ][, by = date, .(newDeaths28DaysByDeathDate = sum(newDeaths28DaysByDeathDate),
                                                                                              newCasesBySpecimenDate = sum(newCasesBySpecimenDate),
                                                                                              newAdmissions = sum(newAdmissions),
                                                                                              areaName = "North East and Yorkshire")]

# Separate all other regions
raw$region_else <- raw$region[!areaName %in% c("East Midlands", "West Midlands", "North East", "Yorkshire and The Humber")]

# Bind Midlands, North East and Yorkshire, other regions
data <- rbind(raw$region_else, raw$midlands, raw$ney)

# Set to NA where data show 0 but are NA; remove admissions column before merge
data <- data[, "newAdmissions" := NULL]

# Merge regional data with regional hospital admissions
data <- merge(data, raw$nhsregion, by = c("date", "areaName"))

# Bind regions with national
data <- rbind(raw$nation, data)
# Keep only England of the nations
data <- data[!data$areaName %in% c("Scotland", "Wales", "Northern Ireland"),]


# Cleaning ----------------------------------------------------------------

# Rename
data <- data.table::as.data.table(data)
old <- unlist(structure)
new <- c("date", "region", "deaths_death",  "cases_test", "cases_hosp")
data <- data.table::setnames(data, old, new)

# Set date sequence to start from 1 Feb (arbitrary)
data$date <- lubridate::ymd(data$date)
data <- data[, .SD[date >= lubridate::ymd("2020-02-01")]]


# Clean environment
rm(old, new, structure, areaType, raw)


# NHS admissions data: from 1 Aug -----------------------------------------
nhs_url <- paste0("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/",
                  lubridate::year(Sys.Date()), "/",
                  ifelse(lubridate::month(Sys.Date())<10, 
                          paste0(0,lubridate::month(Sys.Date())),
                          lubridate::month(Sys.Date())),
                  "/COVID-19-daily-admissions-",
                  gsub("-", "", as.character(Sys.Date()-1)),
                  ".xlsx")

download.file(nhs_url, destfile = paste0("data/", Sys.Date(), "-nhs-admissions.xlsx"), mode = "wb")
rm(nhs_url)

adm_new <- suppressMessages(readxl::read_excel(paste0("data/", Sys.Date(), "-nhs-admissions.xlsx"),
                                   sheet = 1,
                                   range = readxl::cell_limits(c(13, 2), c(21, NA))) %>%
  t() %>%
  tibble::as_tibble(.name_repair = "universal") %>%
  janitor::row_to_names(1) %>%
  dplyr::mutate(date = seq.Date(from = as.Date("2020-08-01"), by = 1, length.out = nrow(.))) %>%
  tidyr::pivot_longer(-date, names_to = "region", values_to = "cases_hosp_new") %>%
  dplyr::mutate(region = ifelse(region == "ENGLAND", "England", region),
                cases_hosp_new = as.numeric(cases_hosp_new)))


# Join NHS and dashboard data ---------------------------------------------
source("utils/utils.R")
data <- dplyr::left_join(data, adm_new, by = c("date", "region")) %>%
  dplyr::mutate(region = factor(region, levels = region_names$region_factor))
