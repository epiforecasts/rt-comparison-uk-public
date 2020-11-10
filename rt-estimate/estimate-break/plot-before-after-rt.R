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
  dplyr::select(-c(date, type, mean, sd, lower_20, upper_20))


compare %>%
  ggplot(aes(x = model)) +
  geom_point(aes(y = median, colour = breakpoint)) +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90, colour = breakpoint)) +
  geom_hline(aes(yintercept = 1), lty = 3) +
  scale_colour_brewer(type = "qual", palette = 7) +
  facet_grid(facets = c("source", "region"), scales = "free") +
  cowplot::theme_cowplot() +
  ggplot2::labs(y = "R(t)", x = NULL) +
  ggplot2::theme(legend.position = "bottom",
                 axis.text.x = element_text())



