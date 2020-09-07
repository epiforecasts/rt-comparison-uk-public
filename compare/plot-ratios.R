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
                caseb_hosp_med, caseb_hosp_l90, caseb_hosp_u90, caseb_hosp_l50, caseb_hosp_u50,
                caseb_deathb_med, caseb_deathb_l90, caseb_deathb_u90, caseb_deathb_l50, caseb_deathb_u50,
                hosp_deathb_med, hosp_deathb_l90, hosp_deathb_u90, hosp_deathb_l50, hosp_deathb_u50)


# Get region names and plotting colours
source("utils/utils.R")


# Scale limits
scale_min <- 0.8
scale_max <- 2
 
# Plot ratios -------------------------------------------------------------

# Cases on deaths
plot_ratio_caseb_deathb <- summary_ratios %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = caseb_deathb_l50, ymax = caseb_deathb_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = caseb_deathb_l90, ymax = caseb_deathb_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = caseb_deathb_l50), alpha = 0.1) +
  geom_line(aes(y = caseb_deathb_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  facet_wrap("region", nrow = 1) +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  #theme(strip.text.x = element_blank()) + ## Removes facet region name
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20)) + 
  labs(y = "Rt(community) / Rt(deaths)", x = "")

# Hospital admissions on deaths
plot_ratio_hosp_deathb <- summary_ratios %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = hosp_deathb_l50, ymax = hosp_deathb_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = hosp_deathb_l90, ymax = hosp_deathb_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = hosp_deathb_l50), alpha = 0.1) +
  geom_line(aes(y = hosp_deathb_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  facet_wrap("region", nrow = 1) +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20)) + 
  labs(y = "Rt(hospital) / Rt(deaths)",  x = "")

# Cases by report date on hospital admissions
plot_ratio_caseb_hosp <- summary_ratios %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = caseb_hosp_l50, ymax = caseb_hosp_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = caseb_hosp_l90, ymax = caseb_hosp_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = caseb_hosp_l50), alpha = 0.1) +
  geom_line(aes(y = caseb_hosp_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  facet_wrap("region", nrow = 1) +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm"),
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20)) + # Keep dates - bottom-most plot in grid
  labs(y = "Rt(community) / Rt(hospital)", x = "")





# England national --------------------------------------------------------


# Cases on deaths
plot_national_ratio_caseb_deathb <- summary_ratios %>%
  dplyr::filter(region %in% "England") %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = caseb_deathb_l50, ymax = caseb_deathb_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = caseb_deathb_l90, ymax = caseb_deathb_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = caseb_deathb_l50), alpha = 0.1) +
  geom_line(aes(y = caseb_deathb_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  labs(y = "", x = "")

# Hospital admissions on deaths
plot_national_hosp_deathb <- summary_ratios %>%
  dplyr::filter(region %in% "England") %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = hosp_deathb_l50, ymax = hosp_deathb_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = hosp_deathb_l90, ymax = hosp_deathb_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = hosp_deathb_l50), alpha = 0.1) +
  geom_line(aes(y = hosp_deathb_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  labs(y = "",  x = "")

# Cases by report date on hospital admissions
plot_national_caseb_hosp <- summary_ratios %>%
  dplyr::filter(region %in% "England") %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = caseb_hosp_l50, ymax = caseb_hosp_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = caseb_hosp_l90, ymax = caseb_hosp_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = caseb_hosp_l50), alpha = 0.1) +
  geom_line(aes(y = caseb_hosp_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) + # Keep dates - bottom-most plot in grid
  labs(y = "", x = "")




