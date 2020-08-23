# Merge plots
source("data/plot-data.R")
source("rt-estimate/plot-rt.R")
source("compare/plot-ratios.R")

region_names <- readRDS("data/region_names.rds")

# National ----------------------------------------------------------------

# Filter data in plots to nations

# Raw data: moving average
plot_ma$data <- plot_ma$data %>%
  dplyr::filter(region %in% region_names$nations) %>%
  dplyr::mutate(region = factor(region, levels = c("England", "Scotland", "Wales", "Northern Ireland")))

# Rt estimates
plot_rt$data <- plot_rt$data %>%
  dplyr::filter(region %in% region_names$nations) %>%
  dplyr::mutate(region = factor(region, levels = c("England", "Scotland", "Wales", "Northern Ireland")))

# Ratios
plot_ratio_caseb_deathb$data <- plot_ratio_caseb_deathb$data %>%
  dplyr::filter(region %in% region_names$nations) %>%
  dplyr::mutate(region = factor(region, levels = c("England", "Scotland", "Wales", "Northern Ireland")))

plot_ratio_caseb_hosp$data <- plot_ratio_caseb_hosp$data %>%
  dplyr::filter(region %in% region_names$nations) %>%
  dplyr::mutate(region = factor(region, levels = c("England", "Scotland", "Wales", "Northern Ireland")))

plot_ratio_hosp_deathb$data <- plot_ratio_hosp_deathb$data %>%
  dplyr::filter(region %in% region_names$nations) %>%
  dplyr::mutate(region = factor(region, levels = c("England", "Scotland", "Wales", "Northern Ireland")))

# Plot all
plot_national <- plot_ma + 
  plot_rt +
  plot_ratio_caseb_deathb +
  plot_ratio_hosp_deathb +
  plot_ratio_caseb_hosp +
  patchwork::plot_layout(nrow=6)

ggsave(here::here("figures", paste0(Sys.Date(), "-national_rt_and_ratios.pdf")),
       plot_national, dpi = 330, height = 10, width = 15)

# Regional ----------------------------------------------------------------
source("data/plot-data.R")
source("rt-estimate/plot-rt.R")
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
       plot_regional, dpi = 330, height = 8, width = 15)


