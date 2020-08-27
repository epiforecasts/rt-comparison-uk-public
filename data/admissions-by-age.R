
# Get age data ------------------------------------------------------------
structure <- list("date", "areaName", 
                  "cumAdmissionsByAge")
names(structure) <- structure
areaType <- list("nhsregion" = "areaType=nhsregion")
# Get data
raw_admissions <- ukcovid19::get_data(filters = areaType, structure = structure) 


# Clean -------------------------------------------------------------------
age_admissions <- tidyr::unnest(raw_admissions, cumAdmissionsByAge) %>%
  # dplyr::filter(areaName == "London") %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>%
  # De-cumulate
  dplyr::group_by(areaName, age) %>%
  dplyr::arrange(date, .by_group = TRUE) %>%
  dplyr::mutate(value = value - lag(value, 1)) %>%
  dplyr::ungroup() %>%
  # Pivot wide
  dplyr::select(-rate) %>%
  tidyr::pivot_wider(id_cols = c("date", "areaName"), names_from = "age", values_from = "value") %>%
  dplyr::rename(region = "areaName", 
                age1864 = "18_to_64", 
                age6584 = "65_to_84",
                age05 = "0_to_5",
                age617 = "6_to_17",
                age85 = "85+") %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(perc_u65 = (age05 + age617 + age1864) / (age1864 + age6584 + age05 + age617 + age85),
                z_perc_u65 = scale(perc_u65, center = TRUE, scale = TRUE),
                ma_perc_u65 = forecast::ma(perc_u65, order = 7)) %>%
  dplyr::ungroup()


# Plot --------------------------------------------------------------------
plot_age <- age_admissions %>%
  ggplot(aes(x = date, col = region, fill = region)) +
  geom_line(aes(y = z_perc_u65)) +
  cowplot::theme_cowplot(font_size = 11) +
  # scale_color_manual(values = colours) +
  theme(panel.spacing.x = unit(0.5, "cm")) +
  # theme(legend.position = "none") +
  # theme(axis.text.x = element_blank()) +
  labs(title = "Hospital admissions, % under 65yrs", y = "% admissions under 65", x = "")

plot(plot_age)
