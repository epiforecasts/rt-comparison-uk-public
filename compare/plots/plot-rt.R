# Plot Rt
library(magrittr); library(ggplot2)

# # Set global variables
# # consistent date axis:
# date_min <- as.Date("2020-03-01")
# date_max <- as.Date("2020-08-20")
# theme_set(theme_classic(base_size = 12))

# Plot Rt --------------------------------------------------------------
plot_rt_fn <- function(region_name){
  summary %>%
    dplyr::filter(region %in% region_name &
                    date >= date_min & date <= date_max) %>%
    ggplot(aes(x = date, col = source, fill = source)) +
    geom_ribbon(aes(ymin = lower_50, ymax = upper_50),
                alpha = 0.3, size = 0, colour = NA) +
    geom_ribbon(aes(ymin = lower_90, ymax = upper_90),
                alpha = 0.2, colour = NA) +
    geom_line(aes(y = median),
              alpha = 0.9, size = 1) +
    geom_vline(xintercept = as.Date("2020-03-23"), 
               lty = 4, colour = colours["Cases"]) +
    geom_vline(xintercept = as.Date("2020-05-03"), 
               lty = 3, colour = colours["Cases"]) +
    geom_hline(yintercept = 1, linetype = 2) +
    coord_cartesian(#ylim = c(scale_min, scale_max),
      xlim = c(date_min, date_max)) +
    scale_color_manual(values = colours) +
    scale_fill_manual(values = colours) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(breaks=seq(0, 1.4, by = 0.2)) +
    cowplot::theme_cowplot() +
    labs(y = expression(R[t]), x = NULL, col = "Data source", fill = "Data source") +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
}


