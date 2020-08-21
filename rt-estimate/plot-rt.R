# Plot Rt
library(magrittr); library(ggplot2)

# Get Rt --------------------------------------------------------------

summary <- readRDS("rt-estimate/summary.rds")

plot_rt <- summary %>%
  ggplot(aes(x = date, col = source, fill = source)) +
  # geom_ribbon(aes(ymin = lower_90, ymax = upper_90),
  #             alpha = 0.1, size = 0.2) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50),
              alpha = 0.1, size = 0.2) +
  geom_hline(yintercept = 1, linetype = 2) +
  #scale_color_manual(values = colours) +
  #scale_fill_manual(values = colours) +
  facet_wrap("region", nrow = 1) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Rt estimates, 50% IQR", y = "R", x = "", col = "Data source", fill = "Data source") +
  theme(legend.position = "top")
