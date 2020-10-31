# Wales & NI under circuit break
# NI: 20-10-16
# W: 20-10-24
# 
library(magrittr)

# Plotting data will take some fixing
source("compare/plots/plot-data.R")
region_plot_data <- purrr::map(region_list, 
                               ~ plot_data_fn(region_name = .x))

plot_vars <- list("nation_order" = c("Wales", "Northern Ireland"),
                  "colours" = c("Test-positives" = "#1b9e77", 
                                "Hospital admissions" =  "#7570b3",
                                "Deaths" = "#d95f02"))
lockdown_date <- tibble::tibble("region" = c("Northern Ireland", "Wales"),
                      "date" = c("2020-10-16", "2020-10-24"))

# Plot Rts
#summary <- readr::read_csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom/collated/rt/summary_latest.csv")

summary_cases <- readr::read_csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom/cases/summary/rt.csv")
summary_adm <- readr::read_csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom/admissions/summary/rt.csv")
summary_deaths <- readr::read_csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom/deaths/summary/rt.csv")
summary <- dplyr::bind_rows(summary_cases, summary_adm, summary_deaths, .id = "source") %>%
  dplyr::mutate('Data source' = dplyr::recode_factor(source, 
                                                     "1" = "Test-positives",
                                                     "2" = "Hospital admissions",
                                                     "3" = "Deaths"),
                region = factor(region, levels = plot_vars$nation_order, ordered = TRUE)) %>%
  tidyr::drop_na(region)



# Plot Rt only
summary %>%
  ggplot2::ggplot(ggplot2::aes(x = date)) +
  ggplot2::geom_rect(ggplot2::aes(xmin=min(summary[summary$type == "estimate based on partial data",]$date),
                                  xmax=max(summary[summary$type == "estimate based on partial data",]$date), 
                                  ymin=min(lower_90)-0.5, ymax=max(upper_90)+0.5), 
                     fill="grey 95", colour = NA) +
  ggplot2::geom_rect(ggplot2::aes(xmin=min(summary[summary$type == "forecast",]$date),
                                  xmax=max(summary[summary$type == "forecast",]$date), 
                                  ymin=min(lower_90)-0.5, ymax=max(upper_90)+0.5), 
                     fill="grey 80", colour = NA) +
  ggplot2::geom_text(label="forecast", ggplot2::aes(x = max(summary[summary$type == "forecast",]$date), y = 0),
                     colour = "grey 60", nudge_x = -2) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_50, ymax = upper_50, 
                                    col = `Data source`, fill = `Data source`),
                       alpha = 0.2, size = 0, colour = NA) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_90, ymax = upper_90,
                                    col = `Data source`, fill = `Data source`),
                        alpha = 0.2, colour = NA) +
  ggplot2::geom_line(ggplot2::aes(y = median, col = `Data source`, fill = `Data source`),
                     alpha = 1, size = 1) +
  ggplot2::geom_vline(aes(xintercept = as.Date(date)), data = lockdown_date, lty = 5) +
  ggplot2::geom_text(label="lockdown", ggplot2::aes(x = as.Date(date), y = 2.5),
                     colour = "dark red", nudge_x = 1,
                     data = lockdown_date) +
  ggplot2::geom_hline(yintercept = 1, linetype = 2) +
  ggplot2::scale_color_manual(values = plot_vars$colours) +
  ggplot2::scale_fill_manual(values = plot_vars$colours) +
  ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  cowplot::theme_cowplot() +
  ggplot2::labs(y = "R(t)", x = NULL) +
  ggplot2::theme(legend.position = "bottom",
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 strip.background = ggplot2::element_blank(),
                 strip.text.x = ggplot2::element_text()) +
  ggplot2::facet_wrap(~ region, ncol = 1) +
  ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

ggsave("wales-ni-firebreak.jpg", width = 10, height = 10)
