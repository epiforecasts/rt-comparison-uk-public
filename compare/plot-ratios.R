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
scale_min <- 0.4
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
  facet_wrap("region", nrow = 1) +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) + # Remove dates
  theme(axis.text.x = element_blank()) + # Remove facet region name
  labs(y = "Ratio", x = "")

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
  facet_wrap("region", nrow = 1) +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) + 
  theme(axis.text.x = element_blank()) + 
  labs(y = "Ratio", x = "")

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
  facet_wrap("region", nrow = 1) +
  cowplot::theme_cowplot() +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) +
  # theme(axis.text.x = element_blank()) + # Keep dates - bottom-most plot in grid
  labs(y = "Ratio", x = "")






