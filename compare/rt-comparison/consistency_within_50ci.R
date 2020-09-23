# Check % of time that one estimate is within the 50% CI of another estimate

source("utils/utils.R")
summary_wide <- readRDS("rt-estimate/summary_wide.rds")

# Summarise consistency by region (median within 50% CI of alternative)
median_within <- summary_wide %>%
  dplyr::filter(region %in% region_names$nhsregions | region %in% "England") %>%
  dplyr::mutate(med_case_admissions = ifelse(median_cases_blend > lower_50_cases_hosp & 
                                               median_cases_blend < upper_50_cases_hosp, 
                                             TRUE, FALSE),
                med_admissions_death_0 = ifelse(median_cases_hosp > lower_50_deaths_blend & 
                                                median_cases_hosp < upper_50_deaths_blend, 
                                             TRUE, FALSE),
                lag_admissions_death_7 = ifelse(median_cases_hosp > dplyr::lead(lower_50_deaths_blend, 7) & 
                                                median_cases_hosp < dplyr::lead(upper_50_deaths_blend, 7), 
                                              TRUE, FALSE),
                lag_admissions_death_14 = ifelse(median_cases_hosp > dplyr::lead(lower_50_deaths_blend, 14) & 
                                                 median_cases_hosp < dplyr::lead(upper_50_deaths_blend, 14), 
                                              TRUE, FALSE),
                med_case_death = ifelse(median_cases_blend > lower_50_deaths_blend & 
                                          median_cases_blend < upper_50_deaths_blend, 
                                              TRUE, FALSE)) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(perc_case_in_admissions = sum(med_case_admissions, na.rm=T) / sum(!is.na(med_case_admissions)),
                   se_case_adm = sqrt(perc_case_in_admissions * (1 - perc_case_in_admissions) / sum(!is.na(med_case_admissions))),
                   l95_case_adm = perc_case_in_admissions - se_case_adm * 1.96,
                   u95_case_adm = perc_case_in_admissions + se_case_adm * 1.96,
                   ##
                   perc_case_in_deaths = sum(med_case_death, na.rm=T) / sum(!is.na(med_case_death)),
                   se_case_death = sqrt(perc_case_in_deaths * (1 - perc_case_in_deaths) / sum(!is.na(med_case_death))),
                   l95_case_death = perc_case_in_deaths - se_case_death * 1.96,
                   u95_case_death = perc_case_in_deaths + se_case_death * 1.96,
                   ##
                   p_med_admissions_death_0 = sum(med_admissions_death_0, na.rm=T) / sum(!is.na(med_admissions_death_0)),
                   se_adm_death = sqrt(p_med_admissions_death_0 * (1 - p_med_admissions_death_0) / sum(!is.na(med_admissions_death_0))),
                   l95_adm_death = p_med_admissions_death_0 - se_adm_death * 1.96,
                   u95_adm_death = p_med_admissions_death_0 + se_adm_death * 1.96,
                   ##
                   p_lag_admissions_death_7 = sum(lag_admissions_death_7, na.rm=T) / sum(!is.na(lag_admissions_death_7)),
                   se_adm_death7 = sqrt(p_lag_admissions_death_7 * (1 - p_lag_admissions_death_7) / sum(!is.na(lag_admissions_death_7))),
                   l95_adm_death7 = p_lag_admissions_death_7 - se_adm_death7 * 1.96,
                   u95_adm_death7 = p_lag_admissions_death_7 + se_adm_death7 * 1.96,
                   ##
                   p_lag_admissions_death_14 = sum(lag_admissions_death_14, na.rm=T) / sum(!is.na(lag_admissions_death_14)),
                   se_adm_death14 = sqrt(p_lag_admissions_death_14 * (1 - p_lag_admissions_death_14) / sum(!is.na(lag_admissions_death_14))),
                   l95_adm_death14 = p_lag_admissions_death_14 - se_adm_death14 * 1.96,
                   u95_adm_death14 = p_lag_admissions_death_14 + se_adm_death14 * 1.96) %>%
  dplyr::select(!starts_with("se"))
                   
                   
readr::write_csv(median_within, "utils/table_1_median_within_50CI.csv")

# Overall summary consistency (median within 50%)
# Run the above but exclude England from region filter
summary_50ci <- tibble::tibble(vars = c("perc_case_in_admissions", "perc_case_in_deaths", 
                                        "p_med_admissions_death_0", "p_lag_admissions_death_7", "p_lag_admissions_death_14"),
               means = c(mean(median_within$perc_case_in_admissions), 
                         mean(median_within$perc_case_in_deaths),
                         mean(median_within$p_med_admissions_death_0),
                         mean(median_within$p_lag_admissions_death_7),
                         mean(median_within$p_lag_admissions_death_14)),
               sd = c(sd(median_within$perc_case_in_admissions), 
                      sd(median_within$perc_case_in_deaths),
                      sd(median_within$p_med_admissions_death_0),
                      sd(median_within$p_lag_admissions_death_7),
                      sd(median_within$p_lag_admissions_death_14)))




# Mean differences --------------------------------------------------------

# Average mean differences by region

agreement <- summary_wide %>%
  select(region, date, starts_with("median")) %>%
  filter(region %in% region_names$nhsregions | region == "England") %>%
  mutate(# Hospital admissions and deaths lag 7
         median_deaths7 = dplyr::lead(median_deaths_blend, 7),
         # Hospital admissions and deaths lag 14
         median_deaths14 = dplyr::lead(median_deaths_blend, 14)) %>%
  group_by(region)


mean_diffs <- function(data, primary, secondary){
  
  colnames(data) <- ifelse(colnames(data) == primary, "primary", colnames(data))
  colnames(data) <- ifelse(colnames(data) == secondary, "secondary", colnames(data))
  
  ttest <- split(data, data$region, drop = TRUE)
  
  est <- purrr::map(ttest, ~ t.test(.x$primary, .x$secondary, paired = TRUE)$estimate) %>%
    bind_rows(.id = "region") %>%
    rename("value" = "mean of the differences") %>%
    mutate(stat = "estimate")
  
  ci <- purrr::map(ttest, ~ t.test(.x$primary, .x$secondary, paired = TRUE)$conf.int) %>%
    purrr::transpose() %>%
    bind_rows(.id = "stat") %>%
    pivot_longer(cols = -stat, names_to = "region") %>%
    bind_rows(est) %>%
    pivot_wider(names_from = stat)
  
  colnames(ci) <- c("region", "l95", "u95", "estimate")
  
  return(ci)
}

cases_deaths <- mean_diffs(agreement, "median_cases_blend", "median_deaths_blend")
cases_adm <- mean_diffs(agreement, "median_cases_blend", "median_cases_hosp")
adm_deaths <- mean_diffs(agreement, "median_cases_hosp", "median_deaths_blend")
adm_deaths_7 <- mean_diffs(agreement, "median_cases_hosp", "median_deaths7")
adm_deaths_14 <- mean_diffs(agreement, "median_cases_hosp", "median_deaths14")




# Plot
min_x = round(min(c(cases_adm$l95, cases_deaths$l95, adm_deaths$l95, adm_deaths_7$l95, adm_deaths_14$l95)), digits = 2)
max_x = round(max(c(cases_adm$u95, cases_deaths$u95, adm_deaths$u95, adm_deaths_7$u95, adm_deaths_14$u95)), digits = 2)


# Plot hospital admissions vs deaths over varying time leads
adm_deaths_join <- bind_rows(adm_deaths, adm_deaths_7, adm_deaths_14, .id = "Lead") %>%
  mutate(region = factor(region, 
                         levels = region_names$region_factor),
         Lead = factor(Lead,
                       labels = c("No lead", "7 day", "14 day"))) %>%
  ggplot(aes(group = region)) +
  geom_point(aes(x = estimate, y = region, colour = region, shape = Lead), 
             group = "Lead",
             position = position_dodge(width = 1), 
             size = 3) +
  geom_linerange(aes(y = region, xmin = l95, xmax = u95, 
                     colour = region, lty = Lead),
                 group = "Lead",
                 position = position_dodge(width = 1)) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  theme_classic() +
  coord_cartesian(xlim = c(min_x, max_x)) +
  labs(x = "Rt(hospital) - Rt(deaths) 
       with varying lead time") +
  scale_color_viridis_d() + 
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        legend.position = "bottom")
  


# Plot community cases vs hospital admissions and deaths Rts

community_mean_diffs <- list(cases_deaths, cases_adm)
names(community_mean_diffs) <- c("Rt(community) - Rt(deaths)", 
                       "Rt(community) - Rt(hospital)")

community_plots <- list()
for(i in 1:length(community_mean_diffs)){
  community_plots[[i]] <- community_mean_diffs[[i]] %>%
  mutate(region = factor(region, 
                         levels = region_names$region_factor)) %>%
  ggplot() +
  geom_point(aes(x = estimate, y = region, colour = region), size = 3) +
  geom_linerange(aes(y = region, xmin = l95, xmax = u95, colour = region)) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  theme_classic() +
  coord_cartesian(xlim = c(min_x, max_x)) +
  labs(x = names(community_mean_diffs[i])) +
  scale_color_viridis_d() + 
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        legend.position = "none")
}


# Join plots
mean_diff_plots <- community_plots[[1]] +
  community_plots[[2]] +
  adm_deaths_join +
  patchwork::plot_layout(guide = "collect", ncol = 3) &
  theme(axis.text.x = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title=element_blank(),
        legend.box = "vertical",
        legend.direction = "horizontal",
        legend.position = "bottom"
        )

plot(mean_diff_plots)


ggsave("figures/mean_diffs.png", mean_diff_plots, 
       height = 8, width = 7)


