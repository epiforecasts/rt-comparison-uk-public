# Plot Rt using daily updated estimates
# Global variables
summary <-  readr::read_csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom/collated/rt/summary_latest.csv") %>%
  dplyr::mutate('Data source' = dplyr::recode_factor(source, 
                                                     "cases" = "Test-positives",
                                                     "admissions" = "Hospital admissions",
                                                     "deaths" = "Deaths"))
colours <- c("Test-positives" = "#1b9e77","Hospital admissions" =  "#7570b3","Deaths" = "#d95f02")
region_names <- factor(c("England", "Scotland", "Wales", "Northern Ireland",
                  "North East and Yorkshire", "North West", "Midlands", "East of England", 
                  "London", "South East", "South West"), ordered = TRUE)
# Plot
plot_rt_fn <- function(region_name){
  summary %>%
    dplyr::filter(region %in% region_name) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, col = `Data source`, fill = `Data source`)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_50, ymax = upper_50),
                alpha = 0.2, size = 0, colour = NA) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_90, ymax = upper_90),
                alpha = 0.1, colour = NA) +
    ggplot2::geom_line(ggplot2::aes(y = median),
              alpha = 0.9, size = 1) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::scale_color_manual(values = colours) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    #scale_y_continuous(breaks=seq(0, 1.4, by = 0.2)) +
    cowplot::theme_cowplot() +
    ggplot2::labs(y = "R", x = NULL, subtitle = region_name) +
    ggplot2::theme(legend.position = "bottom",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
}
region_plot_rt <- purrr::map(region_names,
                             ~ plot_rt_fn(region_name = .x))
# # Plot selected regions:
plot_everywhere <- region_plot_rt[[1]] +
  region_plot_rt[[2]] +
  region_plot_rt[[3]] +
  region_plot_rt[[4]] +
  region_plot_rt[[5]] +
  region_plot_rt[[6]] +
  region_plot_rt[[7]] +
  region_plot_rt[[8]] +
  region_plot_rt[[9]] +
  region_plot_rt[[10]] +
  region_plot_rt[[11]] +
  patchwork::plot_layout(ncol = 1, guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

#ggsave("rt.jpg", height = 20, width = 10)

