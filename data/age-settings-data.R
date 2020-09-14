# Showing a moving epidemic between sub-populations:
# - Age distribution of cases
# - Outbreak settings (care homes, workplaces)

# Data source: national surveillance reports, Public Health England
# URL: https://www.gov.uk/government/publications/national-covid-19-surveillance-reports

library(magrittr); library(ggplot2)

xls_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/914814/Weekly_COVID19_report_data_w36.xlsx"

httr::GET(xls_url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

sheets <- readxl::excel_sheets(tf)

# Set up dates from weeks
dates <- tibble::tibble(week = c(35:5),
                        date = seq.Date(from = as.Date("2020-08-30"), by = -7, length.out = length(c(5:35))))


# By setting --------------------------------------------------------------

#--- Number of acute respiratory infection incidents by institution

setting <- readxl::read_excel(path = tf,
                      sheet = grep("Outbreaks", sheets),
                      range = readxl::cell_limits(c(9, 2), c(NA, NA)))
# Drop unused columns and set colnames
setting <- setting[,1:9]
colnames(setting) <- c("week", "care_home", "hospital", "education", "prison", "workplace", "food", "other", "total")

# Set dates: drop non-covid weeks from last year and attach date
setting <- setting[18:nrow(setting),]
setting <- merge(setting, dates, by = "week")
setting$week <- NULL
setting <- setting %>%
  tidyr::pivot_longer(cols = -c(date, total))

plot_setting <- setting %>%
  ggplot(aes(date, name)) + 
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_classic() +
  labs(x = "", y = "", title = "Weekly number of outbreaks of any size 
       reported by setting in England",
       caption = "Number of confirmed outbreaks of acute respiratory infections 
       i.e. two or more laboratory confirmed cases (COVID-19, influenza or 
       other respiratory pathogen) linked to a particular setting")

ggsave("figures/outbreaks-by-setting.png", plot_setting, width = 5, height = 5)

# By age ------------------------------------------------------------------

# Population by age
# Taken from ONS mid-year 2019: "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"
pop <- tibble::tibble(pop = c(10192089, 21335397, 19981825, 4777650),
                      age = c("a014", "a1544", "a4574", "a75"))

#--- Weekly hospital admission rate per 100,000 positive tests reported through CHESS, by age group
age_admissions_chess <- readxl::read_excel(path = tf,
                              sheet = grep("CHESS - age group", sheets),
                              range = readxl::cell_limits(c(9, 2), c(NA, NA)))
colnames(age_admissions_chess) <- c("week", stringr::str_c("age_", stringr::str_replace_all(colnames(age_admissions_chess[2:length(age_admissions_chess)]), "-", "_")))

# Drop second table (ICU admission rates)
age_admissions_chess <- age_admissions_chess[1:(grep("ICU", age_admissions_chess$age_0_4)-2),]

# Set numeric
age_admissions_chess[,1:8] <- sapply(age_admissions_chess[,1:8], as.numeric)

# Group ages
age_admissions_chess$a014 <- rowSums(age_admissions_chess[2:5])
age_admissions_chess$a1544 <- rowSums(age_admissions_chess[6:8])
age_admissions_chess$a4574 <- rowSums(age_admissions_chess[6:8])
age_admissions_chess$a75 <- rowSums(age_admissions_chess[6:8])


# Total counts from rates
age_admissions_chess[,2:8] <- sapply(age_admissions_chess[,2:8], function(x) x / 100000 * eng_pop)



#--- Join hospital admission rates and positive test rates
age_rates <- dplyr::filter(age_nation, week >= 12) %>%
  dplyr::bind_rows(age_admissions_chess, .id = "Data source") %>%
  dplyr::mutate(`Data source` = ifelse(`Data source` == 1, "Community", "Hospital admissions")) %>%
  tidyr::pivot_longer(cols = -c(week, `Data source`), names_to = "Age") %>%
  dplyr::left_join(dates, by = "week")


plot_age <- age_rates %>%
  ggplot(aes(x = date, group = interaction(Age, `Data source`), colour = Age)) +
  geom_line(aes(y = value, linetype = `Data source`)) +
  theme_classic() +
  facet_wrap("Age") +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        plot.margin = unit(c(1,1,1,1), units = "lines"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12)) +
  labs(y = "Weekly confirmed cases per 100,000 population",
       x = "")

ggsave("figures/rate-by-age-source.png", plot_age, width = 6, height = 5)



# Total and proportion test-positive cases by age over time --------------------------------------------------
xls_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/914814/Weekly_COVID19_report_data_w36.xlsx"

httr::GET(xls_url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

sheets <- readxl::excel_sheets(tf)

case_age_totals <- readxl::read_excel(path = tf,
                              sheet = grep("Cases by age and sex ", sheets),
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
case_age$`20-59` <- rowSums(case_age[,5:8], na.rm=T) / case_age$total_cases * 100
case_age$`60+` <- rowSums(case_age[,9:11], na.rm=T) / case_age$total_cases * 100


plot_case_age <- case_age[,13:16] %>%
  pivot_longer(cols = -date, names_to = "Age") %>%
  ggplot(aes(fill = Age, x = date, y = value)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = NULL, y = "Weekly test-positive cases, %") +
  scale_fill_viridis_d(option = "D", direction = -1) +
  coord_cartesian(xlim = c(min(case_age$date), max(summary_wide$date))) +
  theme(legend.position = "bottom")
  
ggsave("figures/national_testcases_by_age.png")

# Positivity % by PHEC ----------------------------------------------------

source("utils/utils.R")

# Averaged pillar 1 + 2 positivity rates
# 
# # Read in regional positivity rates
# raw_pos_tests <- readxl::read_excel(path = tf,
#                               sheet = grep("Figure 8", sheets),
#                               range = readxl::cell_limits(c(9, 2), c(NA, NA)))
# names(raw_pos_tests)[names(raw_pos_tests)=="...1"] <- "week"
# 
# # Read in England positivity rates
# raw_pos_eng <- readxl::read_excel(path = tf,
#                                   sheet = grep("Figure 5", sheets),
#                                   range = readxl::cell_limits(c(9, 2), c(NA, NA)))
# names(raw_pos_eng) <- c("week", "England")
# 
# 
# # Merge the two tables in each spreadsheet (get rid of table 2 headings)
# na_rows <- which(rowSums(is.na(raw_pos_tests))==length(raw_pos_tests))
# na_rows <- c(na_rows, na_rows + 1, na_rows + 2)
# raw_pos_tests <- raw_pos_tests[-na_rows,]
# 
# na_rows <- which(rowSums(is.na(raw_pos_eng))==length(raw_pos_eng))
# na_rows <- c(na_rows, na_rows + 1, na_rows + 2)
# raw_pos_eng <- raw_pos_eng[-na_rows,1:2]
# 
# # Join regions and England
# raw_pos_tests$week <- as.numeric(raw_pos_tests$week)
# raw_pos_eng$week <- as.numeric(raw_pos_eng$week)
# raw_pos_tests <- cbind(raw_pos_tests, raw_pos_eng[,2])
# 
# pos_tests <- raw_pos_tests %>%
#   dplyr::mutate(dplyr::across(.cols = -week, ~ dplyr::na_if(., "-")),
#                 dplyr::across(.cols = -week, ~ as.numeric(.)),
#                 'Midlands' = (`East Midlands` + `West Midlands`) /2, # aggregate 9 PHE to 7 NHS regions
#                 'North East and Yorkshire' = (`Yorkshire and Humber` + `North East`)/2,
#                 week = as.integer(week)) %>%
#   dplyr::left_join(dates, by = "week") %>%
#   dplyr::select(date, c(England, all_of(region_names$nhsregions))) %>%
#   tidyr::pivot_longer(cols = -date, names_to = "region", values_to = "pos_perc") %>%
#   dplyr::group_by(region, date) %>%
#   dplyr::summarise(pos_perc = mean(pos_perc, na.rm=T))

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



# Care homes --------------------------------------------------------------
# Source for spreadsheet: "https://www.gov.uk/government/statistical-data-sets/covid-19-number-of-outbreaks-in-care-homes-management-information
# A raw copy was downloaded on 2020-09-13 and saved to repo

carehomes <- readxl::read_excel("data/Care_home_outbreaks_of_COVID-19_Management_Information.xlsx", 
                                sheet = "PHE_centres",
                                range = readxl::cell_limits(c(2, 1), c(NA, NA))) %>%
  select(-PHEC15CD) %>%
  pivot_longer(cols = -c('PHE centre', 'Number of care homes'), 
                      names_to = "date", values_to = "outbreaks") %>%
  mutate(date = as.Date(date)) %>%
  drop_na(date) %>%
  rename(region=1, n_carehomes=2, date=3, outbreaks=4)


n_carehomes <- as.data.frame(distinct(carehomes, region, n_carehomes))
Midlands <- data.frame(region = "Midlands")
Midlands$n_carehomes <- n_carehomes$n_carehomes[n_carehomes$region == "East Midlands"] +
  n_carehomes$n_carehomes[n_carehomes$region == "West Midlands"]
NEY <- data.frame(region = "North East and Yorkshire")
NEY$n_carehomes <- n_carehomes[n_carehomes$region == "Yorkshire and Humber","n_carehomes"] +
  n_carehomes[n_carehomes$region == "North East","n_carehomes"]
n_carehomes <- bind_rows(n_carehomes, Midlands, NEY)
n_carehomes <- n_carehomes[n_carehomes$region %in% region_names$value$nhsregions,]

region_n <- c("North East and Yorkshire (n=2238)",
                          "North West (n=1917)",
                          "Midlands (n=3227)",
                          "East of England (n=1726)",
                          "London (n=1385)",
                          "South East (n=2942)",
                          "South West (n=2041)")
region_n_factor <- factor(region_n, levels = region_n)

carehomes_outbreaks <- carehomes %>%
  pivot_wider(-n_carehomes, names_from = region, values_from = outbreaks) %>%
  mutate(Midlands = `East Midlands` + `West Midlands`,
         `North East and Yorkshire` = `Yorkshire and Humber` + `North East`) %>%
  select(date, all_of(region_names$value$nhsregions)) %>%
  pivot_longer(-date,  names_to = "region", values_to = "outbreaks") %>%
  left_join(n_carehomes, by = "region") %>%
  mutate(perc_outbreak = outbreaks / n_carehomes *100,
         region = factor(region, region_names$value$region_factor),
         region_n = factor(paste0(region, " (n=", n_carehomes, ")"), levels = region_n_factor))

full_dates <- dplyr::filter(data, region %in% region_names$value$nhsregions & 
                              date >= min(carehomes_outbreaks$date) &
                              date <= max(carehomes_outbreaks$date)) %>%
  select(date, region) %>%
  left_join(carehomes_outbreaks, by = c("date", "region")) %>%
  group_by(region) %>%
  fill(c(n_carehomes, perc_outbreak), .direction = "down") %>%
  left_join(spring_peaks, by = c("date", "region")) %>%
  mutate(peak_adm = ifelse(source == "hosp", format(as.Date(date, "%m/%d/%Y")), NA),
         peak_deaths = ifelse(source == "deaths", format(as.Date(date, "%m/%d/%Y")), NA),
         region = factor(region, region_names$value$region_factor),
         region_n = factor(region_n, region_n_factor)) 

# Plot
plot_carehomes <- carehomes_outbreaks %>%
  ggplot() +
  geom_line(aes(x = date, y = perc_outbreak)) +
  geom_vline(aes(xintercept = as.Date(peak_adm)), data = full_dates, lty = "dotted", lwd=1) +
  geom_vline(aes(xintercept = as.Date(peak_deaths)), data = full_dates, lty = "dashed", lwd = 1) +
  # geom_point(aes(x = peak_adm, y = perc_outbreak), data = full_dates, shape = 2) +
  # geom_point(aes(x = peak_deaths, y = perc_outbreak), data = full_dates, shape = 3) +
  facet_wrap("region_n", nrow = 2, scales = "free_y") +
  cowplot::theme_cowplot() +
  coord_cartesian(xlim = c(date_min, max(carehomes_outbreaks$date))) +
  scale_color_manual(values = colours) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(panel.spacing.x = unit(0.1, "cm"),
        panel.spacing.y = unit(0.1, "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 15)) +
    # Add label with N
  guides(colour = FALSE) +
  labs(y = "% care homes with new outbreak", x = NULL)

ggsave("figures/carehome_outbreaks.png", height = 4, width = 10)
