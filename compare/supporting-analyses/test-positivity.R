# Positivity % by PHEC ----------------------------------------------------

source("utils/utils.R")

# PHE weekly surveillance report (link needs manual update)
#   eg published 18 September 2020 = week 38
phe_weekly <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/919094/Weekly_COVID19_report_data_w38.xlsx"
tf <- tempfile(fileext = ".xlsx")
httr::GET(phe_weekly, httr::write_disk(tf, overwrite = T))
sheets <- readxl::excel_sheets(tf)

# # Regional pillar 2 test positivity %
raw_pos_region <- readxl::read_excel(path = tf,
                                      sheet = grep("Figure 8", sheets),
                                      range = readxl::cell_limits(c(45, 2), c(NA, NA)))
names(raw_pos_region)[names(raw_pos_region)=="...1"] <- "week"

# Read in England positivity %
raw_pos_eng <- readxl::read_excel(path = tf,
                                  sheet = grep("Figure 5", sheets),
                                  range = readxl::cell_limits(c(45, 2), c(NA, NA)))
raw_pos_eng <- raw_pos_eng[,1:2]
names(raw_pos_eng) <- c("week", "England")

# Join
raw_pos_region$week <- as.numeric(raw_pos_region$week)
raw_pos_eng$week <- as.numeric(raw_pos_eng$week)
raw_pos_tests <- cbind(raw_pos_region, raw_pos_eng[,2])

# Clean: set to PHE regions, add dates
pos_tests <- raw_pos_tests %>%
  dplyr::mutate(dplyr::across(.cols = -week, ~ dplyr::na_if(., "-")),
                dplyr::across(.cols = -week, ~ as.numeric(.)),
                'Midlands' = `East Midlands` + `West Midlands`, # aggregate 9 PHE to 7 NHS regions
                'North East and Yorkshire' = `Yorkshire and Humber` + `North East`) %>%
  week_to_date(.) %>%
  dplyr::rename(date = week_end_date) %>%
  dplyr::select(-week) %>%
  tidyr::pivot_longer(cols = -date, names_to = "region", values_to = "pos_perc")
 
# pos_tests %>%
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = pos_perc)) +
#   facet_wrap("region")