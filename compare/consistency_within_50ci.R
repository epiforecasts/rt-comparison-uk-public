# Check % of time that one estimate is within the 50% CI of another estimate

summary_wide <- readRDS("rt-estimate/summary_wide.rds")

median_within <- summary_wide %>%
  dplyr::filter(region %in% region_names$nhsregions) %>%
  dplyr::mutate(med_case_admissions = ifelse(median_cases_blend > lower_50_cases_hosp & 
                                               median_cases_blend < upper_50_cases_hosp, 
                                             TRUE, FALSE),
                med_admissions_death = ifelse(median_cases_hosp > lower_50_deaths_blend & 
                                                median_cases_hosp < upper_50_deaths_blend, 
                                             TRUE, FALSE),
                lag_admissions_death = ifelse(dplyr::lag(median_cases_hosp, 7) > lower_50_deaths_blend & 
                                                median_cases_hosp < upper_50_deaths_blend, 
                                              TRUE, FALSE),
                med_case_death = ifelse(median_cases_blend > lower_50_deaths_blend & 
                                          median_cases_blend < upper_50_deaths_blend, 
                                              TRUE, FALSE)
                ) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(perc_case_in_admissions = sum(med_case_admissions, na.rm=T) / sum(!is.na(med_case_admissions)),
                   perc_admissions_in_deaths = sum(med_admissions_death, na.rm=T) / sum(!is.na(med_admissions_death)),
                   perc_case_in_deaths = sum(med_case_death, na.rm=T) / sum(!is.na(med_case_death)),
                   lag_admissions_death = sum(lag_admissions_death, na.rm=T) / sum(!is.na(lag_admissions_death)))


mean(median_within$perc_case_in_admissions)
mean(median_within$perc_case_in_deaths)
mean(median_within$perc_admissions_in_deaths)
mean(median_within$lag_admissions_death)
