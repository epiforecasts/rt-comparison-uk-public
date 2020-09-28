# Plot Rt
library(magrittr); library(ggplot2)

# # Set global variables
# # consistent date axis:
# date_min <- as.Date("2020-03-01")
# date_max <- as.Date("2020-08-20")
# theme_set(theme_classic(base_size = 12))

# Get Rt --------------------------------------------------------------

summary <- readRDS("rt-estimate/summary.rds")

# Set data source names
summary <- summary %>%
  dplyr::mutate('Data source' = dplyr::recode_factor(source, 
                                                   "cases_test" = "Test-positive",
                                                   "cases_hosp" = "Hospital admissions",
                                                   "deaths_death" = "Deaths"))
              
# Get region names and plotting colours
source("utils/utils.R")


# Set y-axis
scale_min <- 0.5
scale_max <- 1.3

# # Plot - single Rt plot only
# plot_rt_only <- summary %>%
#   dplyr::filter(date >= date_min & date <= date_max) %>%
#   mutate(date = as.Date(date, format = "%Y%M%D")) %>%
#   ggplot(aes(x = date, col = `Data source`, fill = `Data source`)) +
#   geom_ribbon(aes(ymin = lower_20, ymax = upper_20),
#               alpha = 0.2, size = 0, colour = NA) +
#   geom_ribbon(aes(ymin = lower_50, ymax = upper_50),
#               alpha = 0.1, colour = NA) +
#   geom_line(aes(y = median),
#             alpha = 0.9, size = 1) +
#   geom_hline(yintercept = 1, linetype = 2) +
#   coord_cartesian(#ylim = c(scale_min, scale_max),
#                   xlim = c(date_min, date_max)) +
#   scale_color_manual(values = colours) +
#   scale_fill_manual(values = colours) +
#   facet_wrap("region", nrow = 1, scales = "free_y") +
#   cowplot::theme_cowplot() +
#   theme(panel.spacing.x = unit(0.1, "cm")) +
#   theme(strip.text.x = element_blank()) +
#   #theme(axis.text.x = element_blank()) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b") +
#   labs(y = "R", x = NULL, col = "Data source", fill = "Data source") +
#   theme(legend.position = "none") +
#   guides(fill = guide_legend(override.aes = list(alpha = 1)))
# 


# National settings

plot_rt_national <- summary %>%
  dplyr::filter(region %in% c("England") &
                 date >= date_min & date <= date_max) %>%
  ggplot(aes(x = date, col = `Data source`, fill = `Data source`)) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50),
              alpha = 0.2, size = 0, colour = NA) +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90),
             alpha = 0.1, colour = NA) +
  geom_line(aes(y = median),
              alpha = 0.9, size = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(#ylim = c(scale_min, scale_max),
                  xlim = c(date_min, date_max)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  cowplot::theme_cowplot() +
  labs(y = "R", x = NULL, col = "Data source", fill = "Data source") +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))




# Plot function

plot_rt_fn <- function(region_name){
  summary %>%
    dplyr::filter(region %in% region_name &
                    date >= date_min & date <= date_max) %>%
    ggplot(aes(x = date, col = `Data source`, fill = `Data source`)) +
    geom_ribbon(aes(ymin = lower_50, ymax = upper_50),
                alpha = 0.2, size = 0, colour = NA) +
    geom_ribbon(aes(ymin = lower_90, ymax = upper_90),
                alpha = 0.1, colour = NA) +
    geom_line(aes(y = median),
              alpha = 0.9, size = 1) +
    geom_hline(yintercept = 1, linetype = 2) +
    coord_cartesian(#ylim = c(scale_min, scale_max),
      xlim = c(date_min, date_max)) +
    scale_color_manual(values = colours) +
    scale_fill_manual(values = colours) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(breaks=seq(0, 1.4, by = 0.2)) +
    cowplot::theme_cowplot() +
    labs(y = "R", x = NULL, col = "Data source", fill = "Data source") +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
}


