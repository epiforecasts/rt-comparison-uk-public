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
# #--- Weekly incidence per 100,000 population by age group by region, weeks 31-35
# age_region <- readxl::read_excel(path = tf,
#                                  sheet = grep("Incidence-age region", sheets),
#                                  range = readxl::cell_limits(c(9, 2), c(NA, NA)))
# colnames(age_region) <- c("week", stringr::str_c("age_", stringr::str_replace_all(colnames(age_region[2:length(age_region)]), "-", "_")))
# age_region <- age_region %>%
#   # Clean to get region names
#   dplyr::mutate(region = stringr::str_remove_all(week, "[[:digit:]]"),
#                 region = ifelse(region == "", NA, region),
#                 week = suppressWarnings(as.numeric(week))) %>%
#   tidyr::fill(region, .direction = "down") %>%
#   tidyr::drop_na()

#--- Weekly lab confirmed cases per 100,000 population, Pillar 1 & 2, by age group, national, weeks 5-35
age_nation <- readxl::read_excel(path = tf,
                                 sheet = grep("Case rates by agegrp", sheets),
                                 range = readxl::cell_limits(c(9, 2), c(NA, NA)))
colnames(age_nation) <- c("week", stringr::str_c("age_", stringr::str_replace_all(colnames(age_nation[2:length(age_nation)]), "-", "_")))

# Set numeric
age_nation[,1:8] <- sapply(age_nation[,1:8], as.numeric)

# Join under <64 and >65 ages
age_nation$`<64 yrs` <- rowSums(age_nation[2:5])
age_nation$`65+ yrs` <- rowSums(age_nation[6:8])
age_nation <- age_nation[,c(1,9:10)]

#--- Weekly hospital admission rate per 100,000 positive tests reported through CHESS, by age group
age_admissions_chess <- readxl::read_excel(path = tf,
                              sheet = grep("CHESS - age group", sheets),
                              range = readxl::cell_limits(c(9, 2), c(NA, NA)))
colnames(age_admissions_chess) <- c("week", stringr::str_c("age_", stringr::str_replace_all(colnames(age_admissions_chess[2:length(age_admissions_chess)]), "-", "_")))

# Drop second table (ICU admission rates)
age_admissions_chess <- age_admissions_chess[1:(grep("ICU", age_admissions_chess$age_0_4)-2),]

# Set numeric
age_admissions_chess[,1:8] <- sapply(age_admissions_chess[,1:8], as.numeric)

# Join under <64 and >65 ages
age_admissions_chess$`<64 yrs` <- rowSums(age_admissions_chess[2:5])
age_admissions_chess$`65+ yrs` <- rowSums(age_admissions_chess[6:8])
age_admissions_chess <- age_admissions_chess[,c(1,9:10)]

#--- Join hospital admission rates and positive test rates
age_rates <- dplyr::filter(age_nation, week >= 12) %>%
  dplyr::bind_rows(age_admissions_chess, .id = "Data source") %>%
  dplyr::mutate(`Data source` = ifelse(`Data source` == 1, "Community", "Hospital admissions")) %>%
  tidyr::pivot_longer(cols = -c(week, `Data source`), names_to = "Age") %>%
  dplyr::left_join(dates, by = "week")

# Plot --------------------------------------------------------------------

plot_age <- age_rates %>%
  ggplot(aes(x = date, group = interaction(Age, `Data source`), colour = Age)) +
  geom_line(aes(y = value, linetype = `Data source`)) +
  theme_classic() +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = c(0.85, 0.75)) +
  labs(y = "Weekly confirmed cases per 100,000 population",
       x = "",
       title = "Rate of reported Covid-19 per 100,000 in England, 
       by source of data and age")

ggsave("figures/rate-by-age-source.png", plot_age, width = 5, height = 4)








