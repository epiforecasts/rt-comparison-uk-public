# Plot Rt
library(magrittr); library(ggplot2)

# Get Rt --------------------------------------------------------------

summary <- readRDS("rt-estimate/summary.rds")

# Set data source names
summary <- summary %>%
  dplyr::mutate('Data source' = dplyr::recode_factor(source, 
                                                   "cases_blend" = "Cases",
                                                   "cases_hosp" = "Hospital admissions",
                                                   "deaths_blend" = "Deaths"))
              


# Get region names and plotting colours
source("utils/utils.R")

# Set y-axis
scale_min <- 0
scale_max <- 2.5

# Plot
plot_rt <- summary %>%
  ggplot(aes(x = date, col = `Data source`, fill = `Data source`)) +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90),
             alpha = 0.1, size = 0.2) +
  #geom_ribbon(aes(ymin = lower_50, ymax = upper_50),
  #            alpha = 0.1, size = 0.2) +
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
  labs(subtitle = "Rt estimate",
       y = "R", x = "", col = "Data source", fill = "Data source") +
  theme(legend.position = "none")
