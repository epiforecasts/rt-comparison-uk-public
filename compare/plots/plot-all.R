# Merge plots

#source("rt-estimate/update-format.R")

library(ggplot2); library(magrittr); library(patchwork)

# Get region names and plotting colours
source("utils/utils.R")

# Set global variables
# consistent date axis:

date_min <- as.Date("2020-04-04")
date_max <- max(summary$date)
theme_set(theme_classic(base_size = 15))


# Regional ----------------------------------------------------------------

source("compare/plots/plot-data.R")
source("compare/plots/plot-rt.R")
source("compare/plots/plot-ratios.R")

# Using functions

region_list <- as.list(region_names$region_factor)
names(region_list) <- region_names$region_factor

region_plot_data <- purrr::map(region_list, 
                               ~ plot_data_fn(region_name = .x))
region_plot_rt <- purrr::map(region_list, 
                             ~ plot_rt_fn(region_name = .x))

# 7x2
    
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

ggsave(here::here("figures", paste0(Sys.Date(), "-nation-and-regions.png")),
       plot_everywhere, dpi = 80, height = 18, width = 15)
    




# Old code ----------------------------------------------------------------


# Ratios
plot_ratio_case_death$data <- plot_ratio_case_death$data %>%
  dplyr::filter(date >= date_min &
                  date <= date_max) 

plot_ratio_case_hosp$data <- plot_ratio_case_hosp$data %>%
  dplyr::filter(date >= date_min &
                  date <= date_max)

plot_ratio_hosp_death$data <- plot_ratio_hosp_death$data %>%
  dplyr::filter(date >= date_min &
                  date <= date_max)

plot_regional_ratios <- 
  plot_ratio_case_death +
  plot_ratio_hosp_death +
  plot_ratio_case_hosp +
  plot_layout(nrow=3) +
  plot_annotation(tag_levels = "A") &
  theme(panel.spacing=unit(0.1, "lines"),
        plot.margin = unit(c(0.01,0.01,0.01,0.01), "lines"),
        text = element_text(size = 20))

ggsave(here::here("figures", paste0(Sys.Date(), "-regional_ratios.png")),
       plot_regional_ratios, dpi = 50, height = 12, width = 30)


# Regional data & Rt

# Raw data: moving average
plot_ma_only$data <- plot_ma_only$data %>%
  dplyr::filter(region %in% region_names$nhsregions)

# Rt estimates
plot_rt_only$data <- plot_rt_only$data %>%
  dplyr::filter(region %in% region_names$nhsregions)

# Plot together
plot_rt_data_regional <- plot_ma_only +
  plot_rt_only +
  patchwork::plot_layout(nrow = 4, guides = "collect", heights = c(2,5)) +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(panel.spacing.y = unit(0, "mm"),
        legend.position = "bottom",
        axis.text = element_text(size = 20),
        text = element_text(size = 20))

ggsave(here::here("figures", paste0(Sys.Date(), "-regional_rt_and_data_median.png")),
       plot_rt_data_regional, dpi = 50, height = 10, width = 25)

# Patchwork with plot functions
# # 4 x 4 : 2 rows per region, 2 meta-rows of regions
# {{(region_plot_data[[1]] +
#     region_plot_data[[2]]) /
#  (region_plot_rt[[1]] +
#     region_plot_rt[[2]]) +
#   plot_layout(heights = c(2,5),
#               tag_level = "new")} |
# {(region_plot_data[[3]] +
#    region_plot_data[[4]]) /
#   (region_plot_rt[[3]] +
#     region_plot_rt[[4]]) +
#   plot_layout(heights = c(2,5),
#               tag_level = "new")}} /
# {{(region_plot_data[[5]] +
#     region_plot_data[[6]]) /
#   (region_plot_rt[[5]] +
#     region_plot_rt[[6]]) +
#   plot_layout(heights = c(2,5),
#               tag_level = "new")} |
# {(region_plot_data[[7]] +
#     region_plot_data[[1]]) /
#   (region_plot_rt[[7]] +
#     region_plot_rt[[1]]) +
#   plot_layout(heights = c(2,5),
#               tag_level = "new")}} +
#   plot_layout(guides = "collect") +
#   plot_annotation(tag_levels = c("A", "i")) &
#   theme(legend.position = "bottom")
#  
# England national --------------------------------------------------------
# Reset data
source("compare/plots/plot-data.R")
source("compare/plots/plot-rt.R")
source("compare/plots/plot-ratios.R")

# National - Plot all
plot_national <- 
{plot_ma_only_national +
    plot_rt_national +
    plot_layout(nrow=2, heights = c(3,5))} /
    {plot_national_ratio_case_death +
        plot_national_hosp_death +
        plot_national_case_hosp +
        plot_layout(ncol= 3)} +
  patchwork::plot_annotation(tag_levels = c("A")) +
  plot_layout(nrow = 2, 
              heights = c(5,2),
              guides = "collect") &
  theme(legend.position = "bottom")

ggsave(here::here("figures", paste0(Sys.Date(), "-national_rt_and_ratios_median.png")),
       plot_national, dpi = 90, height = 10, width = 10)
