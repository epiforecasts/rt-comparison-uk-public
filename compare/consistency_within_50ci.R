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


# Mean differences --------------------------------------------------------

# Average mean differences by region

agreement <- summary_wide %>%
  filter(region %in% region_names$nhsregions) %>%
  mutate(# Community cases and deaths
         dif_median_cases_deaths = median_cases_blend - median_deaths_blend,
         dif_lower_90_cases_deaths = lower_90_cases_blend - lower_90_deaths_blend,
         dif_upper_90_cases_deaths = upper_90_cases_blend - upper_90_deaths_blend,
         # Community cases and hospital admissions
         dif_median_cases_admissions = median_cases_blend - median_cases_hosp,
         dif_lower_90_cases_admissions = lower_90_cases_blend - lower_90_cases_hosp,
         dif_upper_90_cases_admissions = upper_90_cases_blend - upper_90_cases_hosp,
         # Hospital admissions and deaths
         dif_median_admissions_deaths = median_cases_hosp - median_deaths_blend,
         dif_lower_90_admissions_deaths = lower_90_cases_hosp - lower_90_deaths_blend,
         dif_upper_90_admissions_deaths = upper_90_cases_hosp - upper_90_deaths_blend,
         # Hospital admissions and deaths lag 7
         dif_median_admissions_deaths7 = median_cases_hosp - dplyr::lag(median_deaths_blend, 7),
         dif_lower_90_admissions_deaths7 = lower_90_cases_hosp - dplyr::lag(lower_90_deaths_blend, 7),
         dif_upper_90_admissions_deaths7 = upper_90_cases_hosp - dplyr::lag(upper_90_deaths_blend, 7),
         # Hospital admissions and deaths lag 14
         dif_median_admissions_deaths14 = median_cases_hosp - dplyr::lag(median_deaths_blend, 14),
         dif_lower_90_admissions_deaths14 = lower_90_cases_hosp - dplyr::lag(lower_90_deaths_blend, 14),
         dif_upper_90_admissions_deaths14 = upper_90_cases_hosp - dplyr::lag(upper_90_deaths_blend, 14)
         )

# Calculate mean difference and 95% CIs by region

agreement_group <- split(agreement, region_names$nhsregions)
agreement_group <- purrr::map(agreement_group, ~ tidyr::drop_na(.x))
agreement_mean <- purrr::map(agreement_group, ~ sapply(.x[,34:length(.x)], mean)) %>%
  bind_rows(.id = "Region") %>%
  mutate(type = "mean")
agreement_sd <- purrr::map(agreement_group, ~ sapply(.x[,34:length(.x)], sd)) %>%
  bind_rows(.id = "Region") %>%
  mutate(type = "sd")
agreement_n <- purrr::map(agreement_group, ~ sapply(.x[,34:length(.x)], length)) %>%
  bind_rows(.id = "Region") %>%
  mutate(type = "n")

agreement_ci <- bind_rows(agreement_mean, agreement_sd, agreement_n) %>%
  tidyr::pivot_longer(cols = -c(Region, type), names_to = "mean_rt_difference") %>%
  tidyr::pivot_wider(names_from = type) %>%
  mutate(mean_l95 = mean - (sd/sqrt(n)) * 1.96,
         mean_u95 = mean + (sd/sqrt(n)) * 1.96,
         type = stringr::str_sub(mean_rt_difference, 1,7),
         Region = factor(Region, levels = region_names$nhsregions))

# Plot
low90 <- agreement_ci %>%
  dplyr::filter(type == "dif_low") %>%
  mutate(mean_rt_difference = factor(mean_rt_difference,
                                     levels = unique(mean_rt_difference),
                                     labels = c("Rt(community)\n-Rt(deaths)",
                                                "Rt(community)\n-Rt(admissions)",
                                                "Rt(admissions)\n-Rt(deaths)",
                                                "Rt(admissions)\n-Rt(deaths) lag 7",
                                                "Rt(admissions)\n-Rt(deaths) lag 14"
                                                ))) %>%
  ggplot(colour = Region, fill = Region) +
  geom_point(aes(x = mean, y = Region, colour = Region)) +
  geom_linerange(aes(y = Region, xmin = mean_l95, xmax = mean_u95, colour = Region)) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  facet_grid("mean_rt_difference", switch = "y") +
  theme_classic() +
  coord_cartesian(xlim = c(-0.2, 0.21)) +
  labs(x = "Lower 90 CI") +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside"
        )

median <- agreement_ci %>%
  dplyr::filter(type == "dif_med") %>%
  ggplot(colour = Region, fill = Region) +
  geom_point(aes(x = mean, y = Region, colour = Region)) +
  geom_linerange(aes(y = Region, xmin = mean_l95, xmax = mean_u95, colour = Region)) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  facet_grid("mean_rt_difference") +
  theme_classic() +
  coord_cartesian(xlim = c(-0.2, 0.21)) +
  labs(x = "Median") +
  theme( axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        strip.text = element_blank(),
        strip.placement = "outside")

up90 <- agreement_ci %>%
  dplyr::filter(type == "dif_upp") %>%
  ggplot(colour = Region, fill = Region) +
  geom_point(aes(x = mean, y = Region, colour = Region)) +
  geom_linerange(aes(y = Region, xmin = mean_l95, xmax = mean_u95, colour = Region)) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  facet_grid("mean_rt_difference") +
  theme_classic() +
  coord_cartesian(xlim = c(-0.2, 0.21)) +
  labs(x = "Upper 90 CI") +
   theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        strip.text = element_blank(),
        strip.placement = "outside"
        )

mean_diffs <- low90 +
  median +
  up90 +
  patchwork::plot_layout(ncol = 3, guides = "collect") +
  patchwork::plot_annotation() &
  theme(legend.position = "bottom",
        legend.title = element_blank())

plot(mean_diffs)


ggsave("figures/mean_diffs.png", mean_diffs, 
       height = 7, width = 7)


# Table 1 column 4
median <- agreement_ci %>%
  dplyr::filter(type == "dif_med") %>%
  group_by(mean_rt_difference) %>%
  summarise(mean_mean = mean(mean),
            sd = sd(mean))
  