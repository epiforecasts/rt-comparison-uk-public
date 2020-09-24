# Merge plots

# source("rt-estimate/update-format.R")

library(ggplot2); library(magrittr); library(patchwork)

# Get region names and plotting colours
source("utils/utils.R")

# Set global variables
# consistent date axis:

date_min <- as.Date("2020-04-04")
date_max <-as.Date("2020-09-16")
theme_set(theme_classic(base_size = 15))


# Regional ----------------------------------------------------------------

source("compare/plots/plot-data.R")
source("compare/plots/plot-rt.R")
source("compare/plots/plot-ratios.R")


# Ratios
plot_ratio_case_death$data <- plot_ratio_case_death$data %>%
  dplyr::filter(region %in% region_names$nhsregions &
                date >= date_min &
                date <= date_max) 

plot_ratio_case_hosp$data <- plot_ratio_case_hosp$data %>%
  dplyr::filter(region %in% region_names$nhsregions &
                  date >= date_min &
                  date <= date_max)

plot_ratio_hosp_death$data <- plot_ratio_hosp_death$data %>%
  dplyr::filter(region %in% region_names$nhsregions &
                  date >= date_min &
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

ggsave(here::here("figures", paste0(Sys.Date(), "-regional_ratios_freey.png")),
       plot_regional_ratios, dpi = 50, height = 15, width = 25)


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
  patchwork::plot_layout(nrow = 2, guides = "collect", heights = c(2,5)) +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(panel.spacing.y = unit(0, "mm"),
        legend.position = "bottom",
        axis.text = element_text(size = 20),
        text = element_text(size = 20))

ggsave(here::here("figures", paste0(Sys.Date(), "-regional_rt_and_data_median.png")),
       plot_rt_data_regional, dpi = 50, height = 10, width = 25)


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
