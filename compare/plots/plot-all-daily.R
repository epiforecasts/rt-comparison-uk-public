# Plot daily updating Rt estimates against each other and data
library(rvest); library(stringr); library(dplyr); library(ggplot2)

# global plotting variables -----------------------------------------------
source("utils/utils.R")
summary <- readr::read_csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom/collated/rt/summary_latest.csv")
# Set data source names and dates
summary <- summary %>%
  dplyr::mutate('Data source' = dplyr::recode_factor(source, 
                                                     "cases" = "Test-positives",
                                                     "admissions" = "Hospital admissions",
                                                     "deaths" = "Deaths"))
date_min <- Sys.Date()-60
date_max <- max(summary$date)

# Set plotting variables
theme_set(theme_classic(base_size = 15))
scale_min <- 0.5
scale_max <- 2.5

# Regions to plot
region_list <- as.list(region_names$region_factor)
names(region_list) <- region_names$region_factor


# get plotting functions --------------------------------------------------
source("compare/plots/plot-rt-daily.R")
source("compare/plots/plot-data.R")

# Plot --------------------------------------------------------------------
# Using functions to plot each region
region_plot_data <- purrr::map(region_list, 
                               ~ plot_data_fn(region_name = .x))
region_plot_rt <- purrr::map(region_list, 
                             ~ plot_rt_fn(region_name = .x))
# Join all plots
plot_everywhere <- (region_plot_data[[1]] + region_plot_rt[[1]] +
                      plot_layout(tag_level = "new",
                                  widths = c(2,5)) &
                      theme(axis.text.x = element_blank())) /
  (region_plot_data[[2]] + region_plot_rt[[2]]+
     plot_layout(tag_level = "new",
                 widths = c(2,5))&
     theme(axis.text.x = element_blank(),
           axis.title.y = element_text())) /
  (region_plot_data[[3]] + region_plot_rt[[3]]+
     plot_layout(tag_level = "new",
                 widths = c(2,5))&
     theme(axis.text.x = element_blank(),
           axis.title.y = element_text())) /
  (region_plot_data[[4]] + region_plot_rt[[4]]+
     plot_layout(tag_level = "new",
                 widths = c(2,5))&
     theme(axis.text.x = element_blank(),
           axis.title.y = element_text())) /
  (region_plot_data[[5]] + region_plot_rt[[5]]+
     plot_layout(tag_level = "new",
                 widths = c(2,5))&
     theme(axis.text.x = element_blank(),
           axis.title.y = element_text())) /
  (region_plot_data[[6]] + region_plot_rt[[6]]+
     plot_layout(tag_level = "new",
                 widths = c(2,5))&
     theme(axis.text.x = element_blank(),
           axis.title.y = element_text())) /
  (region_plot_data[[7]] + region_plot_rt[[7]]+
     plot_layout(tag_level = "new",
                 widths = c(2,5))&
     theme(axis.text.x = element_blank(),
           axis.title.y = element_text())) /
  (region_plot_data[[8]] + region_plot_rt[[8]]+
     plot_layout(tag_level = "new",
                 widths = c(2,5))&
     theme(axis.title.y = element_text())) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = c("A", "1"), 
                  tag_sep = "(", 
                  tag_suffix = ")") &
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        plot.tag = element_text(size = 15))


ggsave(here::here("figures", "daily", paste0(Sys.Date(), "-rt-with-data.png")),
       plot_everywhere, dpi = 80, height = 18, width = 15)



# Plot Rt only ------------------------------------------------------------
summary <- readr::read_csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom/collated/rt/summary_latest.csv")

plot_vars <- list("nation_order" = c("United Kingdom", "England", "Scotland", "Wales", "Northern Ireland"),
                  "nhsregion_order" = c("North East and Yorkshire","North West","Midlands","East of England","London","South East","South West"),
                  "colours" = c("Test-positives" = "#1b9e77", 
                                "Hospital admissions" =  "#7570b3",
                                "Deaths" = "#d95f02"))

summary <- summary %>%
  dplyr::mutate('Data source' = dplyr::recode_factor(source, 
                                                     "cases" = "Test-positives",
                                                     "admissions" = "Hospital admissions",
                                                     "deaths" = "Deaths"),
                region = factor(region, levels = c(plot_vars$nation_order, plot_vars$nhsregion_order)))

# Plot Rt only
summary %>%
  ggplot2::ggplot(ggplot2::aes(x = date, col = `Data source`, fill = `Data source`)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_50, ymax = upper_50),
                       alpha = 0.2, size = 0, colour = NA) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_90, ymax = upper_90),
                       alpha = 0.1, colour = NA) +
  ggplot2::geom_line(ggplot2::aes(y = median),
                     alpha = 0.9, size = 1) +
  ggplot2::geom_hline(yintercept = 1, linetype = 2) +
  ggplot2::scale_color_manual(values = plot_vars$colours) +
  ggplot2::scale_fill_manual(values = plot_vars$colours) +
  ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  ggplot2::scale_y_continuous(breaks=seq(0, 1.4, by = 0.2)) +
  cowplot::theme_cowplot() +
  ggplot2::labs(y = "R", x = NULL) +
  ggplot2::theme(legend.position = "bottom",
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 strip.background = ggplot2::element_blank(),
                 strip.text.x = ggplot2::element_text()) +
  ggplot2::facet_wrap(~ region, ncol = 1) +
  ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))







