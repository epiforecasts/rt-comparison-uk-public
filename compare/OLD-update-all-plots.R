# Plots:
# Linerange of latest; Count data; Rt estimates; Ratios; count, Rt, ratios together
library(patchwork)
# Run prior:
# Update all nowcasts
# update-reshape-rts.R
# update-counts.R
# update-ratios.R

# Set colours
colours <- c(
  "Combined" = "#700A97",
  "Cases" = "#FE5803", 
  "Deaths" = "#0044FB" 
  )
last_estimate_date <- unique(paste(last_estimate$`Day of Value`, last_estimate$`Month of Value`, last_estimate$`Year of Value`,
                            sep = "-"))

# National - linerange ---------------------------------------------------------------

national_linerange <- last_estimate %>%
  filter(Geography %in% nation_names) %>%
  ggplot(aes(x = type, y = Value, col = type)) +
  geom_linerange(aes(ymin = `Quantile 0.05`, ymax = `Quantile 0.95`),
                 alpha = 0.4, size = 5) +
  geom_linerange(aes(ymin = `Quantile 0.25`, ymax = `Quantile 0.75`),
                 alpha = 0.4, size = 5) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(y = "Rt", x = paste0("US national estimate as of ", last_estimate_date), col = "Data")

ggsave(here::here("figures", "national-latest.png"),
       national_linerange, dpi = 300, height = 3, width = 5)


# National - counts -------------------------------------------------------------
# # Plot counts
plot_count_national <- 
  ggplot(count_all_national) +
  geom_line(aes(x = Date, y = z_score, colour = `Data source`)) +
  coord_cartesian(xlim = c(min(ratio_national$date), max(ratio_national$date))) +
  cowplot::theme_cowplot(font_size = 11) +
  scale_color_manual(values = colours) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank()) +
  labs(title = "Standardised counts", y = "z-score", x = "")


# National - Rts -------------------------------------------------------------
# Plot Rts
plot_rt_national <- all_r %>%
  filter(date >= min(ratio_national$date) & date <= max(ratio_national$date)) %>%
  filter(Geography %in% nation_names) %>%
  ggplot(aes(x = date, y = Value, col = type, fill = type)) +
  geom_ribbon(aes(ymin = `Quantile 0.05`, ymax = `Quantile 0.95`),
              alpha = 0.1, size = 0.2) +
  geom_ribbon(aes(col = NULL, ymin = `Quantile 0.25`, ymax = `Quantile 0.75`),
              alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Rt estimates", y = "R", x = "", col = "Data source", fill = "Data source") +
  theme(legend.position = "top")


# National - ratios -------------------------------------------------------------

# # Public tests / admissions
plot_ratio_national <- ratio_national %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = lower50_case_deaths, ymax = upper50_case_deaths),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = lower95_case_deaths, ymax = upper95_case_deaths),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = lower50_case_deaths), alpha = 0.2) +
  geom_line(aes(y = upper50_case_deaths), alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(ylim = c(0.5, 1.5)) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  #theme(strip.text.x = element_blank()) +
  #theme(axis.text.x = element_blank()) +
  labs(title = "Rt(cases) / Rt(deaths)", 
       y = "Ratio", x = "", col = "Data", fill = "Data")


# National - counts, Rts, ratios together -------------------------------------------------------------
plot_all_national <- 
  plot_count_national +
  plot_rt_national +
  plot_ratio_national +
  plot_layout(nrow = 3) +
  theme(legend.position = 'top')

ggsave(here::here("figures", "national_rt_and_ratios.png"),
       plot_all_national, dpi = 330, height = 8, width = 4)



# ### Regional ### --------------------------------------------------------


# Regional - latest linerange -------------------------------------------------------------
regional_linerange <- last_estimate %>%
  filter(Geography %in% region_names) %>%
  ggplot(aes(x = type, y = Value, col = type)) +
  geom_linerange(aes(ymin = `Quantile 0.05`, ymax = `Quantile 0.95`),
                 alpha = 0.4, size = 5) +
  geom_linerange(aes(ymin = `Quantile 0.25`, ymax = `Quantile 0.75`),
                 alpha = 0.4, size = 5) +
  geom_hline(yintercept = 1, linetype = 2) +
  facet_wrap(~ Geography, nrow = 2) + 
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(y = "Rt", x = "", col = "Data")

ggsave(here::here("figures", "regional-latest.png"),
       regional_linerange, dpi = 150, height = 5, width = 12)


# Regional - counts -------------------------------------------------------
plot_count_regional <- 
  ggplot(count_all_regional) +
  geom_line(aes(x = Date, y = z_score, colour = `Data source`)) +
  coord_cartesian(xlim = c(as.Date("2020-03-01"), max(ratio_regional$date))) +
  facet_wrap(~ region, ncol = 7) + 
  cowplot::theme_cowplot(font_size = 11) +
  scale_color_manual(values = colours) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank()) +
  labs(title = "Standardised counts", y = "z-score", x = "")

# Regional - Rts -------------------------------------------------------------
plot_rt_regional <- all_r %>%
  filter(date >= as.Date("202-03-01") & date <= max(ratio_regional$date)) %>%
  filter(Geography %in% region_names) %>%
  ggplot(aes(x = date, y = Value, col = type, fill = type)) +
  geom_ribbon(aes(ymin = `Quantile 0.05`, ymax = `Quantile 0.95`),
              alpha = 0.1, size = 0.2) +
  geom_ribbon(aes(col = NULL, ymin = `Quantile 0.25`, ymax = `Quantile 0.75`),
              alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = 2) +
  facet_wrap(~ Geography, ncol = 7) + 
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) +
  labs(title = "Rt estimates", y = "R", x = "", col = "Data source", fill = "Data source") +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "top")

# Regional - ratios -------------------------------------------------------------
# # Public tests / admissions
plot_regional_test_adm <- ratio_regional %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = lower50_test_adm, ymax = upper50_test_adm),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = lower95_test_adm, ymax = upper95_test_adm),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = lower50_test_adm), alpha = 0.2) +
  geom_line(aes(y = upper50_test_adm), alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(xlim = c(as.Date("2020-03-01"), max(ratio_regional$date)), ylim = c(0.5, 1.5)) +
  facet_wrap(~ region, ncol = 7, scales = "free_x") + 
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Rt(public tests) / Rt(hospital admissions)", y = "Ratio", x = "", col = "Data", fill = "Data")

#  # Public tests / all deaths
plot_regional_test_deaths <- ratio_regional %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = lower50_test_deaths, ymax = upper50_test_deaths),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = lower95_test_deaths, ymax = upper95_test_deaths),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = lower50_test_deaths), alpha = 0.2) +
  geom_line(aes(y = upper50_test_deaths), alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(xlim = c(as.Date("2020-03-01"), max(ratio_regional$date)), ylim = c(0.5, 1.5)) +
  facet_wrap(~ region, ncol = 7, scales = "free_x") + 
  # scale_color_manual(values = colours) +
  # scale_fill_manual(values = colours) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Rt(public tests) / Rt(all deaths)", y = "Ratio", x = "", col = "Data", fill = "Data")


#  # Public tests / hospital deaths
plot_regional_test_hospdeaths <- ratio_regional %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = lower50_test_hospdeaths, ymax = upper50_test_hospdeaths),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = lower95_test_hospdeaths, ymax = upper95_test_hospdeaths),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = lower50_test_hospdeaths), alpha = 0.2) +
  geom_line(aes(y = upper50_test_hospdeaths), alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(xlim = c(as.Date("2020-03-01"), max(ratio_regional$date)), ylim = c(0.5, 1.5)) +
  facet_wrap(~ region, ncol = 7, scales = "free_x") + 
  # scale_color_manual(values = colours) +
  # scale_fill_manual(values = colours) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Rt(public tests) / Rt(hospital deaths)", y = "Ratio", x = "", col = "Data", fill = "Data")

#  # Admissions / all deaths
plot_regional_adm_deaths <- ratio_regional %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = lower50_adm_deaths, ymax = upper50_adm_deaths),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = lower95_adm_deaths, ymax = upper95_adm_deaths),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = lower50_adm_deaths), alpha = 0.2) +
  geom_line(aes(y = upper50_adm_deaths), alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(xlim = c(as.Date("2020-03-01"), max(ratio_regional$date)), ylim = c(0.5, 1.5)) +
  facet_wrap(~ region, ncol = 7, scales = "free_x") + 
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Rt(hospital admissions) / Rt(all deaths)", y = "Ratio", x = "", col = "Data", fill = "Data")

#  # Admissions / hospital deaths
plot_regional_adm_hospdeaths <- ratio_regional %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = lower50_adm_hospdeaths, ymax = upper50_adm_hospdeaths),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = lower95_adm_hospdeaths, ymax = upper95_adm_hospdeaths),
              alpha = 0.1, size = 0.2) +
  geom_line(aes(y = lower50_adm_hospdeaths), alpha = 0.2) +
  geom_line(aes(y = upper50_adm_hospdeaths), alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_cartesian(xlim = c(as.Date("2020-03-01"), max(ratio_regional$date)), ylim = c(0.5, 1.5)) +
  facet_wrap(~ region, ncol = 7, scales = "free_x") + 
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  cowplot::theme_cowplot(font_size = 11) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  theme(strip.text.x = element_blank()) +
  labs(title = "Rt(hospital admissions) / Rt(hospital deaths)", y = "Ratio", x = "", col = "Data", fill = "Data")


# Regional - counts, Rts, ratios together -------------------------------------------------------------
plot_all_regional <- 
  plot_count_regional +
  plot_rt_regional +
  plot_regional_test_adm +
  plot_regional_test_deaths +
  plot_regional_test_hospdeaths +
  plot_regional_adm_deaths +
  plot_regional_adm_hospdeaths +
  patchwork::plot_layout(nrow = 7)

ggsave(here::here("figures", "regional_rt_and_ratios.png"),
       plot_all_regional, dpi = 330, height = 10, width = 12)



