# Filtering Rt estimates for Results section of paper

# Statement:

# ---1---
# "...transmission estimated from all data sources rapidly declined 
#  to an average of one onward case per infection."
library(dplyr)

april_aug <- summary_wide %>%
  filter(region %in% region_names$nhsregions) %>%
  group_by(region) %>%
  filter(date > as.Date("2020-04-01") & date < as.Date("2020-07-31")) %>%
  summarise(mean_cases_blend = mean(upper_90_cases_blend, na.rm=T),
            mean_cases_hosp = mean(upper_90_cases_hosp, na.rm=T),
            mean_deaths_blend = mean(upper_90_deaths_blend, na.rm=T),
            sd_cases_blend = sd(upper_90_cases_blend, na.rm=T),
            sd_cases_hosp = sd(upper_90_cases_hosp, na.rm=T),
            sd_deaths_blend = sd(upper_90_deaths_blend, na.rm=T))

sapply(april_aug[2:7], mean)
sapply(april_aug[2:7], sd)

# between April 1st and July 31st, average 90% CIs were 0.89-1 (SD 0.1, 0.1) estimated from community cases, 
# 0.81-0.95 (SD 0.06, 0.07) from hospital admissions, and 0.71-0.98 (SD 0.07, 0.1) from deaths, over that period.


# Median estimates
# mean_cases_blend   mean_cases_hosp mean_deaths_blend 
# 0.9481905         0.8822262         0.8402262 
# > sapply(april_aug[2:4], sd)
# mean_cases_blend   mean_cases_hosp mean_deaths_blend 
# 0.01557648        0.01337578        0.02093242 

# Lower 50 CI
# > sapply(april_aug[2:4], mean)
# mean_cases_blend   mean_cases_hosp mean_deaths_blend 
# 0.9232500         0.8536071         0.7849524 
# > sapply(april_aug[2:4], sd)
# mean_cases_blend   mean_cases_hosp mean_deaths_blend 
# 0.01683141        0.01459053        0.02989077 

# Upper 50 CI
# mean_cases_blend   mean_cases_hosp mean_deaths_blend 
# 0.9715119         0.9084881         0.8894881 
# > sapply(april_aug[2:4], sd)
# mean_cases_blend   mean_cases_hosp mean_deaths_blend 
# 0.01440771        0.01288944        0.01484779 

# Lower 90 CI
# > sapply(april_aug[2:4], mean)
# mean_cases_blend   mean_cases_hosp mean_deaths_blend 
# 0.8891190         0.8144524         0.7078810 
# > sapply(april_aug[2:4], sd)
# mean_cases_blend   mean_cases_hosp mean_deaths_blend 
# 0.02024388        0.01652816        0.04311209

# Upper 90 CI
# > sapply(april_aug[2:4], mean)
# mean_cases_blend   mean_cases_hosp mean_deaths_blend 
# 1.0076190         0.9509762         0.9765357 
# > sapply(april_aug[2:4], sd)
# mean_cases_blend   mean_cases_hosp mean_deaths_blend 
# 0.01467226        0.01243620        0.01864612 





# ---3---
# From this point there appeared to be an uptick in Rt among 
# any measure of cases in comparison to transmission estimated 
# from cases which resulted in death.












# Bland-Altman plot
library(ggplot2); library(dplyr)
agreement <- summary_wide[summary_wide$region == "England" 
                          & summary_wide$date >= as.Date("2020-03-30") 
                          & summary_wide$date <= as.Date("2020-08-03"),]

agreement$avg_med_case_deaths <- (agreement$median_cases_blend + agreement$median_deaths_blend) / 2

agreement$dif_med_case_deaths <- agreement$median_cases_blend - agreement$median_deaths_blend

ggplot(agreement, aes(x = avg_med_case_deaths, y = dif_med_case_deaths)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = mean(agreement$dif_med_case_deaths, na.rm=T), colour = "blue", size = 0.5) +
  geom_hline(yintercept = mean(agreement$dif_med_case_deaths, na.rm=T) - (1.96 * sd(agreement$dif_med_case_deaths, na.rm=T)), colour = "red", size = 0.5) +
  geom_hline(yintercept = mean(agreement$dif_med_case_deaths, na.rm=T) + (1.96 * sd(agreement$dif_med_case_deaths, na.rm=T)), colour = "red", size = 0.5) +
  ylab("Difference between median Rt estimates") +
  xlab("Average Rt estimate") +
  labs(title = "Limits of agreement between Rt estimates from 
       community cases and deaths") +
  theme_classic()

plot(agreement$median_cases_blend, agreement$median_cases_hosp)
abline(0,1)










