# Positivity % by PHEC ----------------------------------------------------
source("utils/utils.R")
summary <- readRDS("rt-estimate/estimate-all-time/summary_truncated.rds")

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
  tidyr::pivot_longer(cols = -date, names_to = "region", values_to = "pos_perc") %>%
  dplyr::filter(!is.na(pos_perc) & 
                  !region %in% c("West Midlands", "East Midlands",
                                                  "Yorkshire and Humber", "North East") &
                  date <= max(summary$date)) # Filter to match Rt time series
# pos_tests %>%
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = pos_perc)) +
#   facet_wrap("region") +
#   theme_classic()

# Above 5%
pos_over_5 <- pos_tests %>%
  dplyr::filter(pos_perc >= 5)
# NE & Y / Midlands as % of days >5%
nrow(dplyr::filter(pos_over_5, region %in% c("North East and Yorkshire", "Midlands"))) / nrow(pos_over_5)

# Average by region
pos_avg <- split(pos_tests, pos_tests$region) %>%
  purrr::keep(., names(.) %in% c("North East and Yorkshire", 
                                "Midlands", "England")) %>%
  purrr::map(., ~ t.test(.$pos_perc)) %>%
  purrr::map(., ~ purrr::keep(., names(.) %in% c("estimate", "conf.int")))



# Correlate with Rt variables
# # Average by region
peaks_troughs <- readRDS("compare/rt-comparison/peaks_troughs.rds")
region_peaks_valleys <- peaks_troughs$region_peaks_valleys

region_waves_pos <- region_peaks_valleys %>%
  dplyr::filter(source == "cases_test") %>%
  left_join(peaks_troughs$time_between_valleys %>%
              filter(source == "cases_test") %>%
              group_by(region) %>%
              summarise(mean_days_duration = as.numeric(mean(time_between_valleys, na.rm=T))),
            by = "region")

# Comparison to duration of oscillations by source
time_between_all <- peaks_troughs$time_between_valleys %>%
  group_by(region, source) %>%
  summarise(mean_days_duration = as.numeric(mean(time_between_valleys, na.rm=T))) %>%
  left_join(pos_tests %>%
              group_by(region) %>%
              summarise(mean_pos = mean(pos_perc)),
            by = "region")

var <- "deaths_death"
regr_data <- time_between_all %>%
  filter(source == var)
  

# linear regression
pos_predicts_days <- lm(mean_days_duration ~ mean_pos, data = regr_data)
broom::tidy(pos_predicts_days)
broom::glance(pos_predicts_days)
confint(pos_predicts_days)

# cases_hosp: coeff = -1.91 (-5.2 - 1.3) (r2=0.26, p=0.2)
# deaths_death: coeff = -2.13 (-4.6 - 0.3) (r2=0.5, p=0.07)


