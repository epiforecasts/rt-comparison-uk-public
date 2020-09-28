# End of first wave -----------------------------------------------------------------
# Check timing of first wave cross below 1
summary_wide <- readRDS("rt-estimate/summary_wide.rds")

# Cases
rt1_cases_median <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(median_cases_test < 1 & date > "2020-03-23" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, cases = date)

rt1_cases_lower <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(lower_90_cases_test < 1 & date > "2020-03-23" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, cases = date)

rt1_cases_upper <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(upper_90_cases_test < 1 & date > "2020-03-23" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, cases = date)

rt1_cases <- left_join(rt1_cases_median, rt1_cases_lower, by = "region") %>%
  left_join(rt1_cases_upper, by = "region")
colnames(rt1_cases) <- c("region", "cases_median", "cases_lower", "cases_upper")

# Admissions
rt1_hosp_median <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(median_cases_hosp < 1 & date > "2020-03-23" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, cases = date)

rt1_hosp_lower <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(lower_90_cases_hosp < 1 & date > "2020-03-23" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, cases = date)

rt1_hosp_upper <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(upper_90_cases_hosp < 1 & date > "2020-03-23" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, cases = date)

rt1_hosp <- left_join(rt1_hosp_median, rt1_hosp_lower, by = "region") %>%
  left_join(rt1_hosp_upper, by = "region")
colnames(rt1_hosp) <- c("region", "hosp_median", "hosp_lower", "hosp_upper")

# Deaths
rt1_deaths_median <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(median_deaths_death < 1 & date > "2020-03-23" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, cases = date)

rt1_deaths_lower <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(lower_90_deaths_death < 1 & date > "2020-03-23" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, cases = date)

rt1_deaths_upper <- summary_wide %>%
  dplyr::group_by(region) %>%
  dplyr::filter(upper_90_deaths_death < 1 & date > "2020-03-23" & date <= "2020-07-01") %>%
  dplyr::filter(date == min(date)) %>%
  dplyr::select(region, cases = date)

rt1_deaths <- left_join(rt1_deaths_median, rt1_deaths_lower, by = "region") %>%
  left_join(rt1_deaths_upper, by = "region")
colnames(rt1_deaths) <- c("region", "deaths_median", "deaths_lower", "deaths_upper")

# Join to table
rt1 <- left_join(rt1_cases, rt1_hosp, by = "region") %>%
  left_join(rt1_deaths, by = "region") %>%
  mutate(region = factor(region, levels = region_names$region_factor))


# Export
# readr::write_csv(rt1, "compare/rt-comparison/rt-cross-1.csv")

rt1_plot <- rt1 %>%
  ggplot(groups = region) +
  geom_point(aes(x = value, y = name), colour = colours[["Cases"]]) +
  geom_linerange(aes(y = region, xmin = cases_lower, xmax = cases_upper), 
                 colour = colours[["Cases"]]) +
  labs(x = NULL, y = NULL) +
  ggsave("figures/rt-cross-1.png")

# Plot
rt1_plot <- rt1 %>%
  ggplot(groups = region) +
  geom_point(aes(x = cases_median, y = region), colour = colours[["Cases"]]) +
  geom_linerange(aes(y = region, xmin = cases_lower, xmax = cases_upper), 
                 colour = colours[["Cases"]]) +
  geom_point(aes(x = hosp_median, y = region), colour = colours[["Hospital admissions"]],
             position = position_nudge(y = -0.2)) +
  geom_linerange(aes(y = region, xmin = hosp_lower, xmax = hosp_upper), colour = colours[["Hospital admissions"]],
                 position = position_nudge(y = -0.2)) +
  geom_point(aes(x = deaths_median, y = region), colour = colours[["Deaths"]],
             position = position_nudge(y = -0.4)) +
  geom_linerange(aes(y = region, xmin = deaths_lower, xmax = deaths_upper), colour = colours[["Deaths"]],
                 position = position_nudge(y = -0.4)) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(breaks = breaks, 
                    values=c(colours["Cases"], 
                             colours["Hospital admissions"], 
                             colours["Deaths"])) +
  theme(legend.position = "bottom")


  ggsave("figures/rt-cross-1.png")


