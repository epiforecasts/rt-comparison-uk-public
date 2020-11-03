# Format breakpoint estimates and plot
library(data.table); library(magrittr); library(ggplot2); library(patchwork)


break_ni <- as.Date("2020-10-16")
break_wales <- as.Date("2020-10-24")



# Format estimates --------------------------------------------------------
vars <- c("cases", "admissions", "deaths")
models <- c("breakpoint-only",
            "breakpoint-with-rw") #,
            #"gp-breakpoint")
  
# Single breakpoint        
breakpoint_only <- purrr::map(vars, 
                           ~ readr::read_csv(
                            paste0("rt-estimate/estimate-break/", models[1], "/", .x, "/summary/rt.csv")))
names(breakpoint_only) <- c("cases", "admissions", "deaths")
breakpoint_only <- dplyr::bind_rows(breakpoint_only, .id = "source") %>%
  dplyr::mutate(model = "single breakpoint")

# Random walk         
random_walk <- purrr::map(vars, 
                              ~ readr::read_csv(
                                paste0("rt-estimate/estimate-break/", models[2], "/", .x, "/summary/rt.csv")))
names(random_walk) <- c("cases", "admissions", "deaths")
random_walk <- dplyr::bind_rows(random_walk, .id = "source") %>%
  dplyr::mutate(model = "random walk + breakpoint")

# # GP      
# gp <- purrr::map(vars, 
#                  ~ readr::read_csv(
#                                 paste0("rt-estimate/estimate-break/", models[3], "/max-tree-depth-10/", .x, "/summary/rt.csv")))
# names(gp) <- c("cases", "admissions", "deaths")
# gp <- dplyr::bind_rows(gp, .id = "source") %>%
#   dplyr::mutate(model = "gp with breakpoint")
# 
# # Join all & clean
models <- dplyr::bind_rows(breakpoint_only, random_walk) %>%
    dplyr::select(-strat)



# Save to csv -------------------------------------------------------------
readr::write_csv(models, "rt-estimate/estimate-break/firebreak-breakpoints.csv")

# Plot utils --------------------------------------------------------------
colours <- c("cases" = "#1b9e77",  "admissions" =  "#7570b3", "deaths" = "#d95f02")

# Plot data --------------------------------------------------------------------
# get private data
raw <- readRDS("C:/Users/kaths/confidential-data/latest_data.rds")
raw$value_desc <- NULL
data <- raw[raw$type == "Data" ,]
data <- tidyr::pivot_wider(data, values_from = "value", names_from = "value_type")
data$type <- NULL
data <- data[,c("value_date", "geography", "death_inc_line", "hospital_inc", "reported_cases")]
colnames(data) <- c("date", "region", "deaths", "admissions", "cases")
data <- as.data.table(data)
data$date <- lubridate::ymd(data$date)
data <- data[, .SD[date >= (max(date)-42)], by = region]
data <- data[region %in% c("Wales", "Northern Ireland", "South West")]
data <- data[, breakpoint := data.table::fifelse( (date == as.Date("2020-10-16") & 
                                                     region == "Northern Ireland") | 
                                                    (date == as.Date("2020-10-24") & 
                                                       region == "Wales"), 
                                                  1, 0)]
# moving average
data_ma <- data %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(
    # 7-day moving average
    cases = forecast::ma(cases, order = 7), 
    admissions = forecast::ma(admissions, order = 7), 
    deaths = forecast::ma(deaths, order = 7)) %>% 
  dplyr::ungroup() %>%
  tidyr::pivot_longer(-c(region, date, breakpoint), names_to = "source", values_to = "ma")

# Plot function
plot_data_fn <- function(region_name, breakpoint_date = NA){
  data_ma %>%
    dplyr::filter(region %in% region_name) %>%
    ggplot() +
    geom_line(aes(x = date, y = (as.numeric(ma)), 
                  colour = source)) +
    geom_vline(xintercept = as.Date(breakpoint_date), 
               lty = 3, colour = "grey50") +
    cowplot::theme_cowplot() +
    # coord_cartesian(xlim = c(date_min, date_max)) +
    scale_color_manual(values = colours) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
    guides(colour = FALSE) +
    labs(y = "7-day MA", x = NULL, title = region_name)
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
    ggplot2::scale_color_manual(values = colours) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    #scale_y_continuous(breaks=seq(0, 1.4, by = 0.2)) +
    cowplot::theme_cowplot() +
    ggplot2::labs(y = "R(t)", x = NULL) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
}


# Combine plots -----------------------------------------------------------
regions <- unique(as.character(data$region))
break_dates <- c(rep(break_wales, 2), break_ni)

plot_breakpoint <- purrr::map2(.x = regions, .y = break_dates,
                       ~ plot_rt_fn(region_name = .x,
                                  model_name = "single breakpoint",
                                  breakpoint_date = .y))
plot_rw <- purrr::map2(.x = regions, .y = break_dates,
                       ~ plot_rt_fn(region_name = .x,
                                    model_name = "random walk + breakpoint",
                                    breakpoint_date = .y))

data_sw <- plot_data_fn(region_name = "South West", breakpoint_date = break_wales[length(break_wales)])
data_wales <- plot_data_fn(region_name = "Wales", breakpoint_date = break_wales[length(break_wales)])
data_ni <- plot_data_fn(region_name = "Northern Ireland", breakpoint_date = break_ni[length(break_ni)])

# Join plots
plot_breaks <- ((data_sw | data_wales | data_ni) +
                  plot_annotation(subtitle = "A. Data")) /
  ((plot_breakpoint[[1]] | plot_breakpoint[[2]] | plot_breakpoint[[3]]) +
     plot_annotation(subtitle = "B. Single breakpoint")) /
  ((plot_rw[[1]] | plot_rw[[2]] | plot_rw[[3]]) +
     plot_annotation(subtitle = "C. Random walk with breakpoint")) +
  plot_layout(guides = "collect")  &
  theme(legend.position = "bottom")

ggsave(filename = "rt-estimate/estimate-break/sw-wales-ni.png", 
       height = 8, width = 14)

