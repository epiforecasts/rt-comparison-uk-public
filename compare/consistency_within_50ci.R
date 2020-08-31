# Check % of time that one estimate is within the 50% CI of another estimate

source("utils/utils.R")
summary_wide <- readRDS("rt-estimate/summary_wide.rds")

median_within <- summary_wide %>%
  dplyr::filter(region %in% region_names$nhsregions) %>%
  dplyr::mutate(med_case_admissions = ifelse(median_cases_blend > lower_50_cases_hosp & 
                                               median_cases_blend < upper_50_cases_hosp, 
                                             TRUE, FALSE),
                med_admissions_death_0 = ifelse(median_cases_hosp > lower_50_deaths_blend & 
                                                median_cases_hosp < upper_50_deaths_blend, 
                                             TRUE, FALSE),
                lag_admissions_death_7 = ifelse(median_cases_hosp > dplyr::lag(lower_50_deaths_blend, 7) & 
                                                median_cases_hosp < dplyr::lag(upper_50_deaths_blend, 7), 
                                              TRUE, FALSE),
                lag_admissions_death_14 = ifelse(median_cases_hosp > dplyr::lag(lower_50_deaths_blend, 14) & 
                                                 median_cases_hosp < dplyr::lag(upper_50_deaths_blend, 14), 
                                              TRUE, FALSE),
                med_case_death = ifelse(median_cases_blend > lower_50_deaths_blend & 
                                          median_cases_blend < upper_50_deaths_blend, 
                                              TRUE, FALSE)
                ) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(perc_case_in_admissions = sum(med_case_admissions, na.rm=T) / sum(!is.na(med_case_admissions)),
                   perc_case_in_deaths = sum(med_case_death, na.rm=T) / sum(!is.na(med_case_death)),
                   med_admissions_death_0 = sum(med_admissions_death_0, na.rm=T) / sum(!is.na(med_admissions_death_0)),
                   lag_admissions_death_7 = sum(lag_admissions_death_7, na.rm=T) / sum(!is.na(lag_admissions_death_7)),
                   lag_admissions_death_14 = sum(lag_admissions_death_14, na.rm=T) / sum(!is.na(lag_admissions_death_14)))


summary <- tibble::tibble(vars = colnames(median_within[2:6]),
               means = c(mean(median_within$perc_case_in_admissions), 
                         mean(median_within$perc_case_in_deaths),
                         mean(median_within$med_admissions_death_0),
                         mean(median_within$lag_admissions_death_7),
                         mean(median_within$lag_admissions_death_14)),
               sd = c(sd(median_within$perc_case_in_admissions), 
                      sd(median_within$perc_case_in_deaths),
                      sd(median_within$med_admissions_death_0),
                      sd(median_within$lag_admissions_death_7),
                      sd(median_within$lag_admissions_death_14)))

