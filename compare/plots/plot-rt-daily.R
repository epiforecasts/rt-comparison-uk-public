# Plot Rt using daily updated estimates
source("utils/utils.R")

deaths <- readr::read_csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom/deaths/summary/rt.csv")
cases <- readr::read_csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom/cases/summary/rt.csv")
admissions <- readr::read_csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom/admissions/summary/rt.csv")

summary <- dplyr::bind_rows(cases, admissions, deaths, .id = "source") %>%
  dplyr::mutate(`Data source` = dplyr::recode(source, 
                                       "1" = "Test-positive",
                                       "2" = "Hospital admissions",
                                       "3" = "Deaths")) %>%
  dplyr::filter(type == "estimate")

# Set minimum date (1 August, when the NHS hospital data start)
date_min <- min(admissions[admissions$region=="England",]$date)
date_max <- max(summary$date)

region_list <- as.list(region_names$region_factor)
names(region_list) <- region_names$region_factor

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
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(breaks=seq(0, 1.4, by = 0.2)) +
    cowplot::theme_cowplot() +
    labs(y = "R", x = NULL) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
}

region_plot_rt <- purrr::map(region_list, 
                             ~ plot_rt_fn(region_name = .x))

# Plot an individual region:
region_plot_rt$England

# Plot selected regions:
library(patchwork)
plot_everywhere <- region_plot_rt[[1]] +
  region_plot_rt[[2]] +
  region_plot_rt[[3]] +
  region_plot_rt[[4]] +
  region_plot_rt[[5]] +
  region_plot_rt[[6]] +
  region_plot_rt[[7]] +
  region_plot_rt[[8]] +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")



