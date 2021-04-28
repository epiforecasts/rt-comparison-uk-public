# Merge plots
#source("rt-estimate/update-format.R")
library(ggplot2); library(magrittr); library(patchwork); library(rvest); library(stringr)

# Get region names and plotting colours
source("utils/utils.R")

# Get raw data  --------------------------------------------------------------
source("compare/plots/plot-data.R")

# Rt settings -------------------------------------------------------------
source("compare/plots/plot-rt.R")
summary <- readRDS("rt-estimate/estimate-all-time/summary_truncated.rds") %>%
  dplyr::mutate(source = dplyr::recode_factor(source, 
                                              "cases_test" = "Test-positive",
                                              "cases_hosp" = "Hospital admissions",
                                              "deaths_death" = "Deaths"))

# Plot settings ---------------------------------------------------------
# consistent date axis:
date_min <- as.Date("2020-04-04")
date_max <- max(summary$date)
# theme
theme_set(theme_classic(base_size = 15))
# Set y-axis
scale_min <- 0.5
scale_max <- 1.3

# Region plots ------------------------------------------------------------
# Using functions to plot each region
region_list <- as.list(region_names$region_factor)
names(region_list) <- region_names$region_factor

region_plot_data <- purrr::map(region_list, 
                               ~ plot_data_fn(region_name = .x))
region_plot_rt <- purrr::map(region_list, 
                             ~ plot_rt_fn(region_name = .x))

# Join all regions, save
    
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

ggsave(here::here("figures", paste0(Sys.Date(), "-RSTB20200283-figure-1.pdf")),
       plot_everywhere, dpi = "print", height = 18, width = 15)
    



