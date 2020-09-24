# Plot ratios
library(magrittr); library(ggplot2)


# global variables:
# date_min <- as.Date("2020-03-01")
# date_max <-as.Date("2020-08-20")
# theme_set(theme_classic(base_size = 12))

# Get ratios --------------------------------------------------------------

summary_wide <- readRDS("rt-estimate/summary_wide.rds")

summary_ratios <- summary_wide %>%
  dplyr::select(date, region, 
                case_hosp_med, case_hosp_l90, case_hosp_u90, case_hosp_l50, case_hosp_u50,
                case_death_med, case_death_l90, case_death_u90, case_death_l50, case_death_u50,
                hosp_death_med, hosp_death_l90, hosp_death_u90, hosp_death_l50, hosp_death_u50)


# Get region names and plotting colours
source("utils/utils.R")


# Scale limits
scale_min <- 0.8
scale_max <- 2
 
# Plot ratios -------------------------------------------------------------

# Cases on deaths
plot_ratio_case_death <- summary_ratios %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = case_death_l50, ymax = case_death_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = case_death_l90, ymax = case_death_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = case_death_l50), alpha = 0.1) +
  geom_line(aes(y = case_death_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(#ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  facet_wrap("region", nrow = 1, scales = "free_y") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_text(size = 18)) + ## Removes facet region name
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20)) + 
  labs(y = "Rt(test-positive) / Rt(deaths)", x = NULL)

# Hospital admissions on deaths
plot_ratio_hosp_death <- summary_ratios %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = hosp_death_l50, ymax = hosp_death_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = hosp_death_l90, ymax = hosp_death_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = hosp_death_l50), alpha = 0.1) +
  geom_line(aes(y = hosp_death_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(#ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap("region", nrow = 1, scales = "free_y") +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20)) + 
  labs(y = "Rt(hospital) / Rt(deaths)",  x = NULL)

# Cases by report date on hospital admissions
plot_ratio_case_hosp <- summary_ratios %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = case_hosp_l50, ymax = case_hosp_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = case_hosp_l90, ymax = case_hosp_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = case_hosp_l50), alpha = 0.1) +
  geom_line(aes(y = case_hosp_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(#ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap("region", nrow = 1, scales = "free_y") +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm"),
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1)) + # Keep dates - bottom-most plot in grid
  labs(y = "Rt(test-positive) / Rt(hospital)", x = NULL)





# England national --------------------------------------------------------


# Cases on deaths
plot_national_ratio_case_death <- summary_ratios %>%
  dplyr::filter(region %in% "England"&
                  date >= date_min &
                  date <= date_max) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = case_death_l50, ymax = case_death_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = case_death_l90, ymax = case_death_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = case_death_l50), alpha = 0.1) +
  geom_line(aes(y = case_death_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(#ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  labs(y = NULL, x = NULL)

# Hospital admissions on deaths
plot_national_hosp_death <- summary_ratios %>%
  dplyr::filter(region %in% "England" &
                  date >= date_min &
                  date <= date_max) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = hosp_death_l50, ymax = hosp_death_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = hosp_death_l90, ymax = hosp_death_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = hosp_death_l50), alpha = 0.1) +
  geom_line(aes(y = hosp_death_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(#ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  labs(y = NULL,  x = NULL)

# Cases by report date on hospital admissions
plot_national_case_hosp <- summary_ratios %>%
  dplyr::filter(region %in% "England" &
                  date >= date_min &
                  date <= date_max) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = case_hosp_l50, ymax = case_hosp_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = case_hosp_l90, ymax = case_hosp_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = case_hosp_l50), alpha = 0.1) +
  geom_line(aes(y = case_hosp_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(#ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) + # Keep dates - bottom-most plot in grid
  labs(y = NULL, x = NULL)




