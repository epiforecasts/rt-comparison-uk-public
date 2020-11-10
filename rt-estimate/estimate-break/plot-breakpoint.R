# Plot utils --------------------------------------------------------------
colours <- c("cases" = "#1b9e77",  "admissions" =  "#7570b3", "deaths" = "#d95f02")
break_ni <- as.Date("2020-10-16")
break_wales <- as.Date("2020-10-24")

break_dates <- c(rep(break_wales, 2), break_ni)

# Get data
data_list <- readRDS(data_list, here::here("rt-estimate", "estimate-break", Sys.Date(), 
                              "formatted-data.RDS"))
data <- data_list$data
data_ma <- data_list$data_ma

# Get models
models <- readr::read_csv(here::here("rt-estimate", "estimate-break", Sys.Date(), 
                                     "firebreak-breakpoints.csv"))

regions <- unique(as.character(data$region))

# Plot data --------------------------------------------------------------------

# Plot function
plot_data_fn <- function(region_name, breakpoint_date = NA, value_type, title = FALSE){
  data_ma %>%
    dplyr::mutate(source = factor(source)) %>%
    dplyr::filter(region %in% region_name &
                  date >= min(models$date),
                  source == value_type) %>%
    ggplot() +
    geom_line(aes(x = date, y = (as.numeric(ma)),
                  colour = source)) +
    geom_vline(xintercept = as.Date(breakpoint_date),
               lty = 3, colour = "grey50") +
    geom_point(data = data %>%
               dplyr::mutate(source = factor(source)) %>%
               dplyr::filter(region %in% region_name,
                             date >= min(models$date),
                             source == value_type),
               aes(x = date, y = value, colour = source)) +
    cowplot::theme_cowplot() +
    # coord_cartesian(xlim = c(date_min, date_max)) +
    scale_color_manual("", values = colours, drop = FALSE) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
    guides(colour = FALSE) +
      labs(y = value_type, x = NULL) +
      scale_y_log10() -> p
  if (title) {
    p <- p + ggtitle(region_name)
  }
  return(p)
}


# Plot Rt  ----------------------------------------------------------

plot_rt_fn <- function(region_name, model_name, breakpoint_date = NA){
  models %>%
    dplyr::filter(region %in% region_name & 
                    model %in% model_name) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, col = source, fill = source)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_50, ymax = upper_50),
                         alpha = 0.2, size = 0, colour = NA) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_90, ymax = upper_90),
                         alpha = 0.1, colour = NA) +
    ggplot2::geom_line(ggplot2::aes(y = median),
                       alpha = 0.9, size = 1) +
    geom_vline(xintercept = as.Date(breakpoint_date), 
               lty = 3, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::scale_color_manual("", values = colours) +
    ggplot2::scale_fill_manual("", values = colours) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    #scale_y_continuous(breaks=seq(0, 1.4, by = 0.2)) +
    cowplot::theme_cowplot() +
    ggplot2::labs(y = paste("R(t)", model_name), x = NULL) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
}


# Combine plots -----------------------------------------------------------
plot_breakpoint <- purrr::map2(.x = regions, .y = break_dates,
                               ~ plot_rt_fn(region_name = .x,
                                            model_name = "single breakpoint",
                                            breakpoint_date = .y))
plot_rw <- purrr::map2(.x = regions, .y = break_dates,
                       ~ plot_rt_fn(region_name = .x,
                                    model_name = "random walk + breakpoint",
                                    breakpoint_date = .y))

plot_gp <- purrr::map2(.x = regions, .y = break_dates,
                       ~ plot_rt_fn(region_name = .x,
                                    model_name = "Gaussian process",
                                    breakpoint_date = .y))

admissions_sw <- plot_data_fn(region_name = "South West", breakpoint_date = break_wales[length(break_wales)], value_type = "admissions", title = TRUE)
cases_sw <- plot_data_fn(region_name = "South West", breakpoint_date = break_wales[length(break_wales)], value_type = "cases")
deaths_sw <- plot_data_fn(region_name = "South West", breakpoint_date = break_wales[length(break_wales)], value_type = "deaths")

admissions_wales <- plot_data_fn(region_name = "Wales", breakpoint_date = break_wales[length(break_wales)], value_type = "admissions", title = TRUE)
cases_wales <- plot_data_fn(region_name = "Wales", breakpoint_date = break_wales[length(break_wales)], value_type = "cases")
deaths_wales <- plot_data_fn(region_name = "Wales", breakpoint_date = break_wales[length(break_wales)], value_type = "deaths")

admissions_ni <- plot_data_fn(region_name = "Northern Ireland", breakpoint_date = break_ni[length(break_ni)], value_type = "admissions", title = TRUE)
cases_ni <- plot_data_fn(region_name = "Northern Ireland", breakpoint_date = break_ni[length(break_ni)], value_type = "cases")
deaths_ni <- plot_data_fn(region_name = "Northern Ireland", breakpoint_date = break_ni[length(break_ni)], value_type = "deaths")

# Join plots
plot_breaks <- ((admissions_sw | admissions_wales | admissions_ni) +
                  plot_annotation(subtitle = "A. Admissions")) /
  ((cases_sw | cases_wales | cases_ni) +
   plot_annotation(subtitle = "B. Cases")) /
  ((deaths_sw | deaths_wales | deaths_ni) +
   plot_annotation(subtitle = "C. Deaths")) /
  ((plot_breakpoint[[1]] | plot_breakpoint[[2]] | plot_breakpoint[[3]]) +
     plot_annotation(subtitle = "D. Single breakpoint")) /
  ((plot_rw[[1]] | plot_rw[[2]] | plot_rw[[3]]) +
     plot_annotation(subtitle = "E. Random walk with breakpoint")) +
  plot_layout(guides = "collect")  &
  theme(legend.position = "bottom")

# save
saveRDS(plot_breaks, here::here("rt-estimate", "estimate-break", Sys.Date(), "sw-wales-ni.rds"))

ggsave(filename = here::here("rt-estimate", "estimate-break", Sys.Date(), "sw-wales-ni.png"),
       height = 14, width = 14)

