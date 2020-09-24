# Showing a moving epidemic between sub-populations:
# - Age distribution of cases
# - Outbreak settings (care homes, workplaces)

# Data source: national surveillance reports, Public Health England
# URL: https://www.gov.uk/government/publications/national-covid-19-surveillance-reports

library(magrittr); library(ggplot2)

# PHE weekly surveillance report (link needs manual update)
phe_weekly <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/919094/Weekly_COVID19_report_data_w38.xlsx"
tf <- tempfile(fileext = ".xlsx")
httr::GET(phe_weekly, httr::write_disk(tf, overwrite = T))
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
# 
# 