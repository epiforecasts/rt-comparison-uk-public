# Plot Rt before and after breakpoints --------------------------------------------------------------

colours <- c("cases" = "#1b9e77",  "admissions" =  "#7570b3", "deaths" = "#d95f02")
break_ni <- as.Date("2020-10-16")
break_wales <- as.Date("2020-10-24")

# Get models
models <- readr::read_csv(here::here("rt-estimate", "estimate-break", Sys.Date(), "firebreak-breakpoints.csv"))
regions <- unique(as.character(models$region))

compare_ni <- models %>%
  dplyr::filter((date == break_ni-1 | date == break_ni + 1) 
                & region == "Northern Ireland") %>%
  dplyr::group_by(model, source) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(diff_median = median - dplyr::lag(median, 1),
                diff_lower50 = lower_50 - dplyr::lag(lower_50, 1),
                diff_upper50 = upper_50 - dplyr::lag(upper_50, 1))


compare <- models %>%
  dplyr::filter(((date == break_ni-1 | date == break_ni + 1) &
                  region == "Northern Ireland") | 
                ((date == break_wales-1 | date == break_wales + 1) &
                 region %in% c("Wales", "South West"))) %>%
  dplyr::mutate(breakpoint = factor(ifelse(date == break_ni - 1 | date == break_wales -1,
                                    "before", "after"), 
                                    levels = c("before", "after"),
                                    ordered = TRUE)) %>%
  dplyr::select(-c(date, type, mean, sd, lower_20, upper_20)) %>%
  tidyr::pivot_wider(id_cols = c(model, source, region), 
                     names_from = breakpoint, values_from = median:upper_90)

compare %>%
  ggplot(aes(y = model)) +
  geom_point(aes(x = median_before)) +
  geom_point(aes(x = median_after)) +
  geom_linerange(aes(xmin = median_before, xmax = median_after)) +
  geom_linerange(aes(xmin = lower_90_before, xmax = upper_90_after),
                 colour = "grey 50") +
  # # uncertainty
  # geom_jitter(aes(x = lower_90_before), colour = "grey 50", height = 2) +
  # geom_jitter(aes(x = lower_90_after), colour = "grey 50", height = 2) +
  # plotting
  facet_grid(facets = c("source", "region"), scales = "free") +
  cowplot::theme_cowplot() +
  ggplot2::labs(y = "R(t)", x = NULL) +
  ggplot2::theme(legend.position = "bottom")


