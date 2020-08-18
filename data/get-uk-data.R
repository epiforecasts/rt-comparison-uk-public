# Get UK data from which to estimate Rt
library(magrittr)

# Function to get UK data from gov.uk API ---------------------------------

get_uk_data <- function(filters, progress_bar = FALSE) {
  
  api_endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"
  
  variables = list(
    date = "date",
    areaName = "areaName",
    areaCode = "areaCode",
    # Cases by date of specimen
    newCasesBySpecimenDate = "newCasesBySpecimenDate",
    # Cases by date of report
    newCasesByPublishDate = "newCasesByPublishDate",
    # Deaths by date of death
    newDeathsByDeathDate = "newDeathsByDeathDate",
    # Deaths by date of death
    newDeathsByPublishDate = "newDeathsByPublishDate",
    # Tests - all
    newTestsByPublishDate = "newTestsByPublishDate"
  )
  
  results      <- list()
  current_page <- 1
  
  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = 45, style = 3)
  }
  repeat {
    
    response <- httr::VERB("GET", 
                           url = api_endpoint, 
                           query = list(
                             filters = filters, 
                             structure = jsonlite::toJSON(variables, auto_unbox = TRUE, pretty = FALSE),
                             page = current_page), 
                           httr::timeout(20))
    
    if ( response$status_code >= 400 ) {
      err_msg = httr::http_status(response)
      stop(err_msg)
    } else if ( response$status_code == 204 ) {
      break
    }
    
    # Convert response from binary to JSON:
    json_text <- httr::content(response, "text")
    dt        <- jsonlite::fromJSON(json_text)
    results   <- rbind(results, dt$data)
    
    if ( is.null( dt$pagination$`next` ) ){
      break
    }
    
    current_page <- current_page + 1
    
    if (progress_bar) {
      setTxtProgressBar(pb, current_page)
    }
    
  }
  
  return(results)
}


# Return data for UK nations and English regions --------------------------

query_filters <- list(
  nation = "areaType=nation" #,
  # region = "areaType=region"
  )

# Get and combine data for Eng regions and UK nations
data_list <- purrr::map(query_filters, get_uk_data,
                        progress_bar = FALSE)

# Reshape for covidregionaldata
data <- dplyr::bind_rows(data_list$nation, data_list$region) %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>%
  dplyr::rename(region = areaName, ons_code = areaCode,
                cases_datetest = newCasesBySpecimenDate,
                cases_datepublish = newCasesByPublishDate,
                deaths_datedeath = newDeathsByDeathDate,
                deaths_datepublish = newDeathsByPublishDate,
                tests_datepublish = newTestsByPublishDate)
                
library(ggplot2)
data %>%
  ggplot(data, aes(x = date)) +
  geom_line(aes(y = cases_datetest)) 


+
  geom_line(aes(y = cases_datepublish)) +
  geom_line(aes(y = deaths_datedeath)) +
  geom_line(aes(y = deaths_datepublish)) +
  geom_line(aes(y = tests_datepublish))
