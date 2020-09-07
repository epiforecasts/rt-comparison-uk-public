# Merge plots

source("rt-estimate/update-format.R")

library(ggplot2); library(magrittr); library(patchwork)

# Get region names and plotting colours
source("utils/utils.R")

# Set global variables
# consistent date axis:

date_min <- as.Date("2020-03-01")
date_max <-as.Date("2020-08-20")
theme_set(theme_classic(base_size = 12))


# Regional ----------------------------------------------------------------

source("compare/plot-data.R")
source("compare/plot-rt.R")
source("compare/plot-ratios.R")


# Data + Rt
# Raw data: moving average
plot_ma$data <- plot_ma$data %>%
  dplyr::filter(region %in% region_names$nhsregions)
# Rt estimates
plot_rt$data <- plot_rt$data %>%
  dplyr::filter(region %in% region_names$nhsregions) 
plot_regional_data_rt <- 
  plot_ma + 
  plot_rt +
  patchwork::plot_layout(nrow=2, guides = "auto", heights = c(3,6)) &
  theme(panel.spacing=unit(0.1, "lines"),
        plot.margin = unit(c(0.01,0.01,0.01,0.01), "lines"))


# Ratios
plot_ratio_caseb_deathb$data <- plot_ratio_caseb_deathb$data %>%
  dplyr::filter(region %in% region_names$nhsregions) 

plot_ratio_caseb_hosp$data <- plot_ratio_caseb_hosp$data %>%
  dplyr::filter(region %in% region_names$nhsregions)

plot_ratio_hosp_deathb$data <- plot_ratio_hosp_deathb$data %>%
  dplyr::filter(region %in% region_names$nhsregions) 

plot_regional_ratios <- 
  plot_ratio_caseb_deathb +
  plot_ratio_hosp_deathb +
  plot_ratio_caseb_hosp +
  patchwork::plot_layout(nrow=3, guides = "collect") &
  theme(panel.spacing=unit(0.1, "lines"),
        plot.margin = unit(c(0.01,0.01,0.01,0.01), "lines"),
        legend.position = "bottom")



ggsave(here::here("figures", paste0(Sys.Date(), "-regional_rt_and_ratios.pdf")),
       plot_regional, dpi = 150, height = 15, width = 15)

#subtitle = "Rt(all cases) / Rt(deaths)"
#subtitle = "Rt(hospital admissions) / Rt(deaths)", 
#subtitle = "Rt(all cases) / Rt(hospital admissions)", 


# Data and Rt plots -------------------------------------------------------
source("compare/plot-data.R")
source("compare/plot-rt.R")
source("compare/plot-ratios.R")
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
        legend.position = "bottom")

ggsave(here::here("figures", paste0(Sys.Date(), "-regional_rt_and_data.png")),
       plot_rt_data_regional, dpi = 150, height = 9, width = 18)

# Raw data: moving average
plot_ma_only_national$data <- plot_ma_only_national$data %>%
  dplyr::filter(region %in% c("England")) %>%
  dplyr::mutate(region = factor(region, levels = "England", labels = "England"))
# Rt estimates
plot_rt_national$data <- plot_rt_national$data %>%
  dplyr::filter(region %in% c("England")) %>%
  dplyr::mutate(region = factor(region, levels = "England", labels = "England"))
# Plot together
plot_rt_data_national <- plot_ma_only_national +
  plot_rt_national +
  patchwork::plot_layout(nrow = 2, guides = "collect", heights = c(2,5)) +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(panel.spacing.y = unit(0, "mm"),
        legend.position = "bottom")

plot_nation_region <- (plot_ma_only_national / plot_rt_national ) +
  (plot_ma_only / plot_rt_only ) +
  patchwork::plot_layout(guides = "collect", heights = c(4,4,10)) +
  patchwork::plot_annotation(tag_levels = "A") 
  
  
  plot_rt_data_regional +
  patchwork::plot_layout(ncol = 2, guides = "auto") +
  patchwork::plot_annotation(tag_levels = "1")




# England national ----------------------------------------------------------------
source("compare/plot-data.R")
source("compare/plot-rt.R")
source("compare/plot-ratios.R")

# Filter data in plots 
region_names$nations <- c("England")

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
plot_national <- (plot_ma +
  plot_rt_national ) / {
  plot_ratio_caseb_deathb +
  plot_ratio_hosp_deathb +
  plot_ratio_caseb_hosp +
  patchwork::plot_layout(ncol = 1) }

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
