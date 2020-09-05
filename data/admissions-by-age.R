
# Get age data ------------------------------------------------------------
structure <- list("date", "areaName", 
                  "cumAdmissionsByAge")
names(structure) <- structure
areaType <- list("nhsregion" = "areaType=nhsregion")

# Get data
raw_admissions <- ukcovid19::get_data(filters = areaType, structure = structure) 


# Clean -------------------------------------------------------------------
age_admissions <- tidyr::unnest(raw_admissions, cumAdmissionsByAge) %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>%
  # De-cumulate
  dplyr::group_by(areaName, age) %>%
  dplyr::arrange(date, .by_group = TRUE) %>%
  dplyr::mutate(value = value - dplyr::lag(value, 1),
                rate = rate - dplyr::lag(rate, 1)) %>%
  dplyr::ungroup() %>%
  # Pivot wide
  dplyr::select(-value) %>%
  tidyr::pivot_wider(id_cols = c("date", "areaName"), names_from = "age", values_from = "rate") %>%
  dplyr::rename(region = "areaName", 
                age1864 = "18_to_64", 
                age6584 = "65_to_84",
                age05 = "0_to_5",
                age617 = "6_to_17",
                age85 = "85+") %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(young_old = age1864 / (age6584+age85)) %>%
  dplyr::ungroup()

  # dplyr::mutate(perc_u65 = (age05 + age617 + age1864) / (age1864 + age6584 + age05 + age617 + age85),
  #               z_perc_u65 = scale(perc_u65, center = TRUE, scale = TRUE),
  #               ma_perc_u65 = forecast::ma(perc_u65, order = 7)) %>%
  # dplyr::ungroup()


# Plot --------------------------------------------------------------------
library(ggplot2)
plot_age <- age_admissions %>%
  ggplot(aes(x = date, col = region, fill = region)) +
  geom_line(aes(y = age1864)) +
  cowplot::theme_cowplot(font_size = 11) +
  # scale_color_manual(values = colours) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  # theme(legend.position = "none") +
  # theme(axis.text.x = element_blank()) +
  labs(title = "Hospital admissions among 18-65yrs", y = "Rate per 100,000", x = "")

plot(plot_age)
