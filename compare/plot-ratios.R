# Plot ratios
library(magrittr); library(ggplot2)

# Get ratios --------------------------------------------------------------

summary_wide <- readRDS("rt-estimate/summary_wide.rds")

summary_ratios <- summary_wide %>%
  dplyr::select(date, region, 
                pub_hosp_med, pub_hosp_l90, pub_hosp_u90, pub_hosp_l50, pub_hosp_u50,
                pub_deaths_med, pub_deaths_l90, pub_deaths_u90, pub_deaths_l50, pub_deaths_u50,
                hosp_deaths_med, hosp_deaths_l90, hosp_deaths_u90, hosp_deaths_l50, hosp_deaths_u50)


# Plot ratios -------------------------------------------------------------
facet_rows <- 1

# Cases by report date on hospital admissions
plot_ratio_pub_hosp <- summary_ratios %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = pub_hosp_l50, ymax = pub_hosp_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = pub_hosp_l90, ymax = pub_hosp_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = pub_hosp_l50), alpha = 0.1) +
  geom_line(aes(y = pub_hosp_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(ylim = c(-0.5,3)) +
  #scale_color_manual(values = colours) +
  #scale_fill_manual(values = colours) +
  facet_wrap(~ region, nrow = facet_rows) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  #theme(strip.text.x = element_blank()) +
  #theme(axis.text.x = element_blank()) +
  labs(title = "Rt(all cases) / Rt(admissions)", 
       y = "Ratio", x = "")


# Cases by report date on deaths
plot_ratio_pub_deaths <- summary_ratios %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = pub_deaths_l50, ymax = pub_deaths_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = pub_deaths_l90, ymax = pub_deaths_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = pub_deaths_l50), alpha = 0.1) +
  geom_line(aes(y = pub_deaths_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(ylim = c(-0.5,3)) +
  #scale_color_manual(values = colours) +
  #scale_fill_manual(values = colours) +
  facet_wrap(~ region, nrow = facet_rows) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  #theme(strip.text.x = element_blank()) +
  #theme(axis.text.x = element_blank()) +
  labs(title = "Rt(all cases) / Rt(deaths)", 
       y = "Ratio", x = "")

# Hospital admissions on deaths
plot_ratio_hosp_deaths <- summary_ratios %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = hosp_deaths_l50, ymax = hosp_deaths_u50),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = hosp_deaths_l90, ymax = hosp_deaths_u90),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = hosp_deaths_l50), alpha = 0.1) +
  geom_line(aes(y = hosp_deaths_u50), alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(ylim = c(-0.5,3)) +
  #scale_color_manual(values = colours) +
  #scale_fill_manual(values = colours) +
  facet_wrap(~ region, nrow = facet_rows) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  #theme(strip.text.x = element_blank()) +
  #theme(axis.text.x = element_blank()) +
  labs(title = "Rt(hospital admissions) / Rt(deaths)", 
       y = "Ratio", x = "")




