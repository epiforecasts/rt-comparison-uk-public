# Merge plots

source("rt-estimate/update-format.R")

library(ggplot2); library(magrittr); library(patchwork)

# Get region names and plotting colours
source("utils/utils.R")

# Set global variables
# consistent date axis:
date_min <- as.Date("2020-03-01")
date_max <- as.Date("2020-08-20")
theme_set(theme_classic(base_size = 12))


# Regional ----------------------------------------------------------------

source("compare/plot-data.R")
source("compare/plot-rt.R")
source("compare/plot-ratios.R")

# Raw data: moving average
plot_ma$data <- plot_ma$data %>%
  dplyr::filter(region %in% region_names$nhsregions)
# Rt estimates
plot_rt$data <- plot_rt$data %>%
  dplyr::filter(region %in% region_names$nhsregions) 
# Ratios
plot_ratio_caseb_deathb$data <- plot_ratio_caseb_deathb$data %>%
  dplyr::filter(region %in% region_names$nhsregions) 

plot_ratio_caseb_hosp$data <- plot_ratio_caseb_hosp$data %>%
  dplyr::filter(region %in% region_names$nhsregions)

plot_ratio_hosp_deathb$data <- plot_ratio_hosp_deathb$data %>%
  dplyr::filter(region %in% region_names$nhsregions) 

# All
plot_regional <- plot_ma + 
  plot_rt +
  plot_ratio_caseb_deathb +
  plot_ratio_hosp_deathb +
  plot_ratio_caseb_hosp +
  patchwork::plot_layout(nrow=6)

ggsave(here::here("figures", paste0(Sys.Date(), "-regional_rt_and_ratios.pdf")),
       plot_regional, dpi = 150, height = 15, width = 15)

# Plot Rt and data
plot_rt_data <- plot_ma_only +
  plot_rt_only +
  patchwork::plot_layout(ncol = 3)

ggsave(here::here("figures", paste0(Sys.Date(), "-regional_rt_and_data.pdf")),
       plot_rt_data, dpi = 150, height = 15, width = 15)


# England national ----------------------------------------------------------------
source("compare/plot-data.R")
source("compare/plot-rt.R")
source("compare/plot-ratios.R")

# Filter data in plots 
region_names$nations <- c("England", "Scotland")

# Raw data: moving average
plot_ma$data <- plot_ma$data %>%
  dplyr::filter(region %in% region_names$nations)

# Rt estimates
plot_rt_national$data <- plot_rt_national$data %>%
  dplyr::filter(region %in% region_names$nations)

# Ratios
plot_ratio_caseb_deathb$data <- plot_ratio_caseb_deathb$data %>%
  dplyr::filter(region %in% region_names$nations)
plot_ratio_caseb_hosp$data <- plot_ratio_caseb_hosp$data %>%
  dplyr::filter(region %in% region_names$nations)
plot_ratio_hosp_deathb$data <- plot_ratio_hosp_deathb$data %>%
  dplyr::filter(region %in% region_names$nations)

# Plot all
plot_national <- plot_ma +
  plot_rt_national +
  plot_ratio_caseb_deathb +
  plot_ratio_hosp_deathb +
  plot_ratio_caseb_hosp +
  patchwork::plot_layout(nrow=6)
# 
# plot_a <-  plot_ma +
#   plot_rt_national +
#   patchwork::plot_layout(nrow=1)
# 
# plot_b <-
#   plot_ratio_caseb_deathb +
#   plot_ratio_hosp_deathb +
#   plot_ratio_caseb_hosp +
#   patchwork::plot_layout(nrow=1)
# 
# plot_a + 
#   plot_b 


ggsave(here::here("figures", paste0(Sys.Date(), "-national_rt_and_ratios.png")),
       plot_national, dpi = 330, height = 12, width = 11)
