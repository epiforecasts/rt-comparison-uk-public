# Merge plots

# source("rt-estimate/update-format.R")

 
# Get region names and plotting colours
source("utils/utils.R")

# National ----------------------------------------------------------------
# source("compare/plot-data.R")
# source("compare/plot-rt.R")
# source("compare/plot-ratios.R")

# # Filter data in plots to nations
# 
# # Raw data: moving average
# plot_ma$data <- plot_ma$data %>%
#   dplyr::filter(region %in% region_names$nations) 
# 
# # Rt estimates
# plot_rt$data <- plot_rt$data %>%
#   dplyr::filter(region %in% region_names$nations) 
# 
# # Ratios
# plot_ratio_caseb_deathb$data <- plot_ratio_caseb_deathb$data %>%
#   dplyr::filter(region %in% region_names$nations) 
# 
# plot_ratio_caseb_hosp$data <- plot_ratio_caseb_hosp$data %>%
#   dplyr::filter(region %in% region_names$nations) 
# 
# plot_ratio_hosp_deathb$data <- plot_ratio_hosp_deathb$data %>%
#   dplyr::filter(region %in% region_names$nations) 
# 
# # Plot all
# plot_national <- plot_ma + 
#   plot_rt +
#   plot_ratio_caseb_deathb +
#   plot_ratio_hosp_deathb +
#   plot_ratio_caseb_hosp +
#   patchwork::plot_layout(nrow=6)
# 
# ggsave(here::here("figures", paste0(Sys.Date(), "-national_rt_and_ratios.pdf")),
#        plot_national, dpi = 330, height = 10, width = 15)

# Regional ----------------------------------------------------------------

# Set global variables
# consistent date axis:
date_min <- as.Date("2020-03-01")
date_max <- as.Date("2020-08-20")
theme_set(theme_classic(base_size = 12))

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


