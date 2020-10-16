# Showing a moving epidemic between sub-populations:
# - Age distribution of cases
# - Outbreak settings (care homes, workplaces)

# Data source: national surveillance reports, Public Health England
# URL: https://www.gov.uk/government/publications/national-covid-19-surveillance-reports

library(magrittr); library(ggplot2); library(ggsci); library(tidyr)

summary <- readRDS("rt-estimate/estimate-all-time/summary_truncated.rds")

seq_dates <- tibble::tibble(
  date = seq.Date(from = as.Date("2020-01-01"), length.out = 365, by = 1),
  days = weekdays(date),
  week = lubridate::epiweek(date)) %>%
  dplyr::filter(days == "Saturday") %>%
  dplyr::select(week, date)


# PHE weekly surveillance report (link needs manual update)
phe_weekly <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/919094/Weekly_COVID19_report_data_w38.xlsx"
tf <- tempfile(fileext = ".xlsx")
httr::GET(phe_weekly, httr::write_disk(tf, overwrite = T))
sheets <- readxl::excel_sheets(tf)

# # Set up dates from weeks
# dates <- tibble::tibble(week = c(35:5),
#                         date = seq.Date(from = as.Date("2020-08-30"), by = -7, length.out = length(c(5:35))))


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
age_admissions_chess <- age_admissions_chess[1:min(which(is.na(age_admissions_chess$...1)))-1,]

# Group ages
age_admissions_chess$a014 <- rowSums(age_admissions_chess[2:3])
age_admissions_chess$a1544 <- rowSums(age_admissions_chess[4])
age_admissions_chess$a4574 <- rowSums(age_admissions_chess[5:6])
age_admissions_chess$a75 <- rowSums(age_admissions_chess[7:8])

age_admissions_chess_transform <- age_admissions_chess[,9:12]

# get total cases from rates
age_admissions_chess_transform$`0-14` <- (age_admissions_chess_transform$a014 / 100000 * dplyr::pull(pop[pop$age=="a014",2]))
age_admissions_chess_transform$`15-44` <- (age_admissions_chess_transform$a1544 / 100000 * dplyr::pull(pop[pop$age=="a1544",2]))
age_admissions_chess_transform$`45-74` <- (age_admissions_chess_transform$a4574 / 100000 * dplyr::pull(pop[pop$age=="a4574",2]))
age_admissions_chess_transform$`75+` <- (age_admissions_chess_transform$a75 / 100000 * dplyr::pull(pop[pop$age=="a75",2]))

age_admissions_chess_transform$total_cases <- rowSums(age_admissions_chess_transform[,5:8])

# % of total per week, by age group
age_admissions_chess_transform$`0-14` <- age_admissions_chess_transform$`0-14` / age_admissions_chess_transform$total_cases * 100
age_admissions_chess_transform$`15-44` <- age_admissions_chess_transform$`15-44` / age_admissions_chess_transform$total_cases * 100
age_admissions_chess_transform$`45-74` <- age_admissions_chess_transform$`45-74` / age_admissions_chess_transform$total_cases * 100
age_admissions_chess_transform$`75+` <- age_admissions_chess_transform$`75+` / age_admissions_chess_transform$total_cases * 100

# From weeks to dates starting from end of "week 12"
age_admissions_chess_transform$date <- seq.Date(from = as.Date("2020-03-21"),
                                                by = 7,
                                                length.out = nrow(age_admissions_chess_transform))


# Sum over ages 0-44
age_admissions_chess_transform$young <- rowSums(age_admissions_chess_transform[,c("0-14", "15-44")])




# All test positive cases by age ------------------------------------------

case_age_totals <- readxl::read_excel(path = tf,
                              sheet = grep("Cases by age and sex", sheets),
                              range = readxl::cell_limits(c(10, 2), c(NA, NA)))

# set colnames
colnames(case_age_totals) <- c("week", 
                               "a5", "a9", "a19", 
                               "a29", "a39", "a49", 
                               "a59", "a69", 
                               "a79", "a80p")
case_age_totals$week <- as.numeric(case_age_totals$week)
case_age_totals <- case_age_totals[case_age_totals$week >= 6 &
                                     !is.na(case_age_totals$week),]

case_age <- suppressWarnings(case_age_totals %>%
  group_by(week) %>%
  mutate(across(.cols = dplyr::everything(), ~ as.numeric(.))) %>%
  summarise(across(.cols = a5:dplyr::last_col(), ~ sum(.))))

case_age <- merge(case_age, seq_dates, by = "week")
case_age$total_cases <- rowSums(case_age[,c("a5", "a9", "a19", 
                                            "a29", "a39", "a49", 
                                            "a59", "a69",
                                            "a79", "a80p")], na.rm=T)
case_age$`0-19` <- rowSums(case_age[,c("a5", "a9", "a19")], na.rm=T) / case_age$total_cases * 100
case_age$`20-49` <- rowSums(case_age[,c("a29", "a39", "a49")], na.rm=T) / case_age$total_cases * 100
case_age$`50-69` <- rowSums(case_age[,c("a59", "a69")], na.rm=T) / case_age$total_cases * 100
case_age$`70+` <- rowSums(case_age[,c("a79", "a80p")], na.rm=T) / case_age$total_cases * 100

case_age$young <- rowSums(case_age[,c("0-19", "20-49")], na.rm=T)



# Plot --------------------------------------------------------------------

plot_chess_age <- age_admissions_chess_transform[,c(5:8, 10)] %>%
  pivot_longer(cols = -date, names_to = "Age") %>%
  ggplot(aes(fill = Age, x = date, y = value)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = NULL, y = "Weekly hospital admissions, %") +
  #scale_fill_hue(h = c(200, 300), l = 30, c = 200, direction = -1) +
  scale_fill_viridis_d(option = 5, direction = -1) +
  coord_cartesian(xlim = c(min(age_admissions_chess_transform$date), max(summary$date))) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

plot_case_age <- case_age[,c(12,14:17)] %>%
  pivot_longer(cols = -date, names_to = "Age") %>%
  ggplot(aes(fill = Age, x = date, y = value)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = NULL, y = "Weekly test-positive cases, %") +
  #scale_fill_hue(h = c(200, 300), l = 45, c = 170, direction = -1) +
  scale_fill_viridis_d(direction = -1) +
  coord_cartesian(xlim = c(min(case_age$date), max(summary$date))) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title.align = 0)

# Combine plots
plot_case_age +
  plot_chess_age +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

ggsave(filename = paste0("figures/", Sys.Date(), "_national_cases_by_age.png"), height = 3, width = 7)


# Compare -----------------------------------------------------------------
# Week on week difference in proportions
# admissions
age_admissions_compare <- age_admissions_chess_transform %>%
  filter(date <= max(summary$date)) %>%
  select('0-14':'75+', date) %>%
  pivot_longer(-date, names_to = "age", values_to = "percent") %>%
  group_by(age) %>%
  mutate(week_diff = percent - dplyr::lag(percent, 1),
         month = lubridate::month(date),
         half = ifelse(month %in% c(3,4,5), 1,
                       2))

age_admissions_month <- age_admissions_compare %>%
  group_by(half, age) %>%
  summarise(mean_change = mean(week_diff),
            month_min = min(percent),
            month_max = max(percent),
            range = month_max - month_min,
            mean_percent = mean(percent),
            sd_percent = sd(percent))

# Compare to Rt
eng <- summary[summary$region == "England",]

eng_month <- mutate(eng, month = lubridate::month(date)) %>%
  group_by(month, region, source) %>%
  summarise(mean = mean(median),
            sd = sd(median))

# Test-positive cases
case_age_compare <- case_age %>%
  select(date:`70+`, -total_cases) %>%
  tidyr::pivot_longer(-date, names_to = "age", values_to = "percent") %>%
  group_by(age) %>%
  mutate(running_diff = percent - dplyr::lag(percent, 1)) %>%
  filter(date <= max(summary$date))

# Increase among 0-19 before and after 
case_age_compare %>%
  mutate(period = ifelse(date < as.Date("2020-05-16"), 
                       "March - mid-May", 
                       ifelse(date < as.Date("2020-08-01"),
                              "mid-May - end-July",
                              "August"))) %>%
  group_by(period, age) %>%
  summarise(mean_diff = mean(running_diff, na.rm=T),
            mean_perc = mean(percent))

# Compare with Rt
summary_wide <- summary %>%
  select(-type) %>%
  tidyr::pivot_wider(id_cols = c(date, region),
                     names_from = source, values_from = c(lower_90:median)) %>%
  mutate(median_cases_deaths = median_cases_test / median_deaths_death,
         median_cases_admissions = median_cases_test / median_cases_hosp,
         median_case_death_diff = abs(median_cases_test - median_deaths_death)) %>%
  select(everything(), date)

eng_match_case_death <- dplyr::filter(summary_wide, 
                                      region == "England" &
                                        date > "2020-05-23" &
                                        date < "2020-08-09") %>%
  select(date, median_cases_test, lower_90_cases_test, upper_90_cases_test,
         median_deaths_death, lower_90_deaths_death, upper_90_deaths_death,
         median_cases_deaths) 
max(eng_match_case_death$median_cases_deaths)
t.test(eng_match_case_death$median_cases_deaths)

