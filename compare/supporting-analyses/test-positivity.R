# Positivity % by PHEC ----------------------------------------------------

source("utils/utils.R")

# Averaged pillar 1 + 2 positivity rates
# 
# # Read in regional positivity rates
raw_pos_tests <- readxl::read_excel(path = tf,
                                    sheet = grep("Figure 8", sheets),
                                    range = readxl::cell_limits(c(9, 2), c(NA, NA)))
names(raw_pos_tests)[names(raw_pos_tests)=="...1"] <- "week"


# Merge the two tables in each spreadsheet (get rid of table 2 headings)
na_rows <- which(rowSums(is.na(raw_pos_tests))==length(raw_pos_tests))
na_rows <- c(na_rows, na_rows + 1, na_rows + 2)
raw_pos_tests <- raw_pos_tests[-na_rows,]

na_rows <- which(rowSums(is.na(raw_pos_eng))==length(raw_pos_eng))
na_rows <- c(na_rows, na_rows + 1, na_rows + 2)
raw_pos_eng <- raw_pos_eng[-na_rows,1:2]

# Join regions and England
raw_pos_tests$week <- as.numeric(raw_pos_tests$week)
raw_pos_eng$week <- as.numeric(raw_pos_eng$week)
raw_pos_tests <- cbind(raw_pos_tests, raw_pos_eng[,2])

pos_tests <- raw_pos_tests %>%
  dplyr::mutate(dplyr::across(.cols = -week, ~ dplyr::na_if(., "-")),
                dplyr::across(.cols = -week, ~ as.numeric(.)),
                'Midlands' = (`East Midlands` + `West Midlands`) /2, # aggregate 9 PHE to 7 NHS regions
                'North East and Yorkshire' = (`Yorkshire and Humber` + `North East`)/2,
                week = as.integer(week)) %>%
  dplyr::left_join(dates, by = "week") %>%
  dplyr::select(date, c(England, all_of(region_names$nhsregions))) %>%
  tidyr::pivot_longer(cols = -date, names_to = "region", values_to = "pos_perc") %>%
  dplyr::group_by(region, date) %>%
  dplyr::summarise(pos_perc = mean(pos_perc, na.rm=T))

phe_weekly <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/914814/Weekly_COVID19_report_data_w36.xlsx"

httr::GET(phe_weekly, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

sheets <- readxl::excel_sheets(tf)

# # Regional pillar 2
raw_pos_pillar2 <- readxl::read_excel(path = tf,
                                      sheet = grep("Figure 8", sheets),
                                      range = readxl::cell_limits(c(44, 2), c(NA, NA)))
colnames(raw_pos_pillar2) <- colnames(pos_pillar1)

# Read in England positivity rates
raw_pos_eng <- readxl::read_excel(path = tf,
                                  sheet = grep("Figure 5", sheets),
                                  range = readxl::cell_limits(c(44, 2), c(NA, NA)))
raw_pos_eng <- raw_pos_eng[,1:2]
names(raw_pos_eng) <- c("week", "England")

# Join
raw_pos_pillar2$week <- as.numeric(raw_pos_pillar2$week)
raw_pos_eng$week <- as.numeric(raw_pos_eng$week)
raw_pos_tests <- cbind(raw_pos_pillar2, raw_pos_eng[,2])

pos_tests <- raw_pos_tests %>%
  dplyr::mutate(dplyr::across(.cols = -week, ~ dplyr::na_if(., "-")),
                dplyr::across(.cols = -week, ~ as.numeric(.)),
                'Midlands' = `East Midlands` + `West Midlands`, # aggregate 9 PHE to 7 NHS regions
                'North East and Yorkshire' = `Yorkshire and Humber` + `North East`) %>%
  dplyr::left_join(dates, by = "week") %>%
  dplyr::select(date, all_of(region_names$nhsregions)) %>%
  tidyr::pivot_longer(cols = -date, names_to = "region", values_to = "pos_perc")


full_date <- tibble::tibble(
  date = seq.Date(from = min(pos_pillar2_threshold_min$date)-7,
                  to = max(pos_pillar2_threshold_min$date),
                  by = 1)) %>%
  dplyr::left_join(pos_pillar2_threshold_min, by = "date")

# 
# pos_pillar2 %>%
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = positivity_perc)) +
#   facet_wrap("region")