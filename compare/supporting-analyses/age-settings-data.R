# Showing a moving epidemic between sub-populations:
# - Age distribution of cases
# - Outbreak settings (care homes, workplaces)

# Data source: national surveillance reports, Public Health England
# URL: https://www.gov.uk/government/publications/national-covid-19-surveillance-reports

library(magrittr); library(ggplot2)

phe_weekly <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/916993/Weekly_COVID19_Surveillance_Report_week_37_FINAL.pdf"

httr::GET(phe_weekly, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

sheets <- readxl::excel_sheets(tf)

# Set up dates from weeks
dates <- tibble::tibble(week = c(35:5),
                        date = seq.Date(from = as.Date("2020-08-30"), by = -7, length.out = length(c(5:35))))


# By age ------------------------------------------------------------------

# Population by age
# Taken from ONS mid-year 2019: "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"
pop <- tibble::tibble(age = as.character(c("a014", "a1544", "a4574", "a75", "total")),
                  pop = as.numeric(c(10192089, 21335397, 19981825, 4777650, 56286961)))

#--- Weekly hospital admission rate per 100,000 positive tests reported through CHESS, by age group
age_admissions_chess <- readxl::read_excel(path = tf,
                              sheet = grep("CHESS - age group", sheets),
                              range = readxl::cell_limits(c(9, 2), c(NA, NA)),
                              col_types = "numeric")

# Drop second table (ICU admission rates)
age_admissions_chess <- age_admissions_chess[1:24,]

# Group ages
age_admissions_chess$a014 <- rowSums(age_admissions_chess[2:3])
age_admissions_chess$a1544 <- rowSums(age_admissions_chess[4])
age_admissions_chess$a4574 <- rowSums(age_admissions_chess[5:6])
age_admissions_chess$a75 <- rowSums(age_admissions_chess[7:8])

age_admissions_chess_transform <- age_admissions_chess[,9:12]


age_admissions_chess_transform$`0-14` <- (age_admissions_chess_transform$a014 / 100000 * dplyr::pull(pop[pop$age=="a014",2]))
age_admissions_chess_transform$`15-44` <- (age_admissions_chess_transform$a1544 / 100000 * dplyr::pull(pop[pop$age=="a1544",2]))
age_admissions_chess_transform$`45-74` <- (age_admissions_chess_transform$a4574 / 100000 * dplyr::pull(pop[pop$age=="a4574",2]))
age_admissions_chess_transform$`75+` <- (age_admissions_chess_transform$a75 / 100000 * dplyr::pull(pop[pop$age=="a75",2]))
age_admissions_chess_transform$total_cases <- rowSums(age_admissions_chess_transform[,5:8])


age_admissions_chess_transform$`0-14` <- age_admissions_chess_transform$`0-14` / age_admissions_chess_transform$total_cases * 100
age_admissions_chess_transform$`15-44` <- age_admissions_chess_transform$`15-44` / age_admissions_chess_transform$total_cases * 100
age_admissions_chess_transform$`45-74` <- age_admissions_chess_transform$`45-74` / age_admissions_chess_transform$total_cases * 100
age_admissions_chess_transform$`75+` <- age_admissions_chess_transform$`75+` / age_admissions_chess_transform$total_cases * 100

age_admissions_chess_transform$week <- age_admissions_chess$...1
age_admissions_chess_transform <- merge(age_admissions_chess_transform, dates, by = "week")

age_admissions_chess_transform$young <- rowSums(age_admissions_chess_transform[,6:7])



plot_chess_age <- age_admissions_chess_transform[,c(11, 6:9)] %>%
  pivot_longer(cols = -date, names_to = "Age") %>%
  ggplot(aes(fill = Age, x = date, y = value)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = NULL, y = "Weekly hospital admissions, %") +
  scale_fill_viridis_d(option = "D", direction = -1) +
  coord_cartesian(xlim = c(min(age_admissions_chess_transform$date), max(summary_wide$date))) +
  theme(legend.position = "bottom",
        legend.title = element_blank())


#--- test-positive cases by age 

case_age_totals <- readxl::read_excel(path = tf,
                              sheet = grep("Cases by age and sex", sheets),
                              range = readxl::cell_limits(c(10, 2), c(NA, NA)))

# set colnames
colnames(case_age_totals) <- c("week", "a5", "a9", "a19", "a29", "a39", "a49", "a59", "a69", "a79", "a80")
case_age_totals$week <- as.numeric(case_age_totals$week)
case_age_totals <- case_age_totals[case_age_totals$week >= 6 &
                                     !is.na(case_age_totals$week),]

case_age <- case_age_totals %>%
  group_by(week) %>%
  mutate(across(.cols = 1:10, ~ as.numeric(.))) %>%
  summarise(across(.cols = 1:10, ~ sum(.)))

case_age <- merge(case_age, dates, by = "week")
case_age$total_cases <- rowSums(case_age[,2:11], na.rm=T)
case_age$`0-19` <- rowSums(case_age[,2:4], na.rm=T) / case_age$total_cases * 100
case_age$`20-49` <- rowSums(case_age[,5:7], na.rm=T) / case_age$total_cases * 100
case_age$`50-69` <- rowSums(case_age[,8:9], na.rm=T) / case_age$total_cases * 100
case_age$`70+` <- rowSums(case_age[,10:11], na.rm=T) / case_age$total_cases * 100

case_age$young <- rowSums(case_age[,14:15], na.rm=T)

plot_case_age <- case_age[,c(12,14:17)] %>%
  pivot_longer(cols = -date, names_to = "Age") %>%
  ggplot(aes(fill = Age, x = date, y = value)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = NULL, y = "Weekly test-positive cases, %") +
  scale_fill_viridis_d(option = "D", direction = -1) +
  coord_cartesian(xlim = c(min(case_age$date), max(summary_wide$date))) +
  theme(legend.position = "bottom",
        legend.title.align = 0)
  

# Combine plots
plot_case_age +
  plot_chess_age +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("figures/national_cases_by_age.png")


# Compare to Rt
eng <- summary_wide[summary_wide$region == "England",]
eng <- merge(eng, case_age[,c(12,18)], by = "date", all.x = TRUE)
# 
# wave_case_hosp <- wave_features(eng, variable = "case_hosp_med", window = 7)
# wave_case_death <- wave_features(eng, variable = "case_death_med", window = 7)
# wave_hosp_death <- wave_features(eng, variable = "hosp_death_med", window = 7)

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
# 
# 
# pos_pillar2 %>%
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = positivity_perc)) +
#   facet_wrap("region")
# 
# 