# Compare case counts and oscillations

# Crossing points ---------------------------------------------------------
# Function taken from tsfeatures::crossing_points()
cross_points <- function (x) 
{
  midline <- median(x, na.rm = TRUE)
  ab <- x <= midline
  lenx <- length(x)
  p1 <- ab[1:(lenx - 1)]
  p2 <- ab[2:lenx]
  cross <- (p1 & !p2) | (p2 & !p1)
  return(cross)
}

source("utils/utils.R")

summary <- readRDS("rt-estimate/summary.rds")
summary_split <- split(summary, summary$region)

# adjust dates that differ for england v regions
purrr::map(summary_split, ~ min(.x$date))
max_date <- purrr::map(summary_split, ~ max(.x$date)) %>%
  purrr::map(., ~ min(.x)) %>%
  unlist() %>%
  lubridate::as_date() %>%
  min()

# Get crossing points
summary_group <- summary %>%
  dplyr::group_by(region, source) %>%
  dplyr::arrange(date, .by_group = TRUE) %>%
  dplyr::mutate(date_n = seq_along(date)) %>%
  dplyr::filter(date_n <= 161) %>%
  dplyr::mutate(cross = c(cross_points(median), NA)) %>%
  dplyr::select(date, region, cross, date_n,
                median_mid = median, lower_90, upper_90,
                source) %>%
  # Create groups between median crosses
  dplyr::mutate(cross_group = as.character(ifelse(cross == FALSE, 
                                                  NA, seq_along(cross)))) %>%
  tidyr::fill(cross_group, .direction = "up") %>%
  dplyr::group_by(region, source, cross_group) %>%
  dplyr::mutate(cross_min = min(median_mid),
                cross_max = lubridate::as_date(ifelse(median_mid == max(median_mid),
                                                      date, NA)))

# Cut into 25 (7-day) equal tiles
region_peak <- summary %>%
  dplyr::group_by(region, source) %>%
  dplyr::arrange(date, .by_group = TRUE) %>%
  dplyr::mutate(date_n = seq_along(date)) %>%
  dplyr::mutate(tile = ifelse(date_n %in% round(seq(0, 172, length.out = 25)), 
                              date_n, NA)) %>%
  tidyr::fill(tile, .direction = "up") %>%
  dplyr::group_by(region, source, tile) %>%
  dplyr::slice_max(median) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(region, source) %>%
  dplyr::mutate(date_diff = date_n - dplyr::lag(date_n, 1)) %>%
  dplyr::filter(date_diff > 7)

# plot
summary %>%
  dplyr::filter(source == "cases_test") %>%
  ggplot(aes(x = date, colour = source)) +
  geom_line(aes(y = median)) +
  geom_vline(aes(xintercept = date, colour = source), data = region_peak %>%
               dplyr::filter(source == "cases_test")) +
  facet_wrap(~ region)



# Plot Rt
plot_cross_fn <- function(region_name){
  summary_join %>%
    dplyr::mutate(cross = ifelse(cross == TRUE, date, NA)) %>%
    dplyr::filter(region %in% region_name) %>%
    ggplot(aes(x = date, col = source, fill = source)) +
    geom_ribbon(aes(ymin = lower_50, ymax = upper_50),
                alpha = 0.2, size = 0, colour = NA) +
    geom_ribbon(aes(ymin = lower_90, ymax = upper_90),
                alpha = 0.1, colour = NA) +
    geom_line(aes(y = median),
              alpha = 0.9, size = 1) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_line(aes(y = midpt)) +
    geom_vline(xintercept = cross) +
    # coord_cartesian(#ylim = c(scale_min, scale_max),
    #   xlim = c(date_min, date_max)) +
    scale_color_manual(values = colours) +
    scale_fill_manual(values = colours) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(breaks=seq(0, 1.4, by = 0.2)) +
    cowplot::theme_cowplot() +
    labs(y = "R", x = NULL, col = "Data source", fill = "Data source") +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
}

plot_cross_fn("England")

# Get count data ----------------------------------------------------------
# If data won't download, read in a saved hard copy of cleaned data - 23 August 2020
if(class(try(source(here::here("data", "get-uk-data.R")))) == "try-error") {
  message("--- API failure - loading saved data ---")
  data <- readRDS("data/200922.rds")
} else {
  source(here::here("data", "get-uk-data.R"))
}

data_group <- data %>%
  dplyr::group_by(region) %>%
  dplyr::arrange(date, .by_group = TRUE) %>%
  dplyr::mutate(date_n = seq_along(date)) %>%
  dplyr::filter(date_n <= 161)


# Join --------------------------------------------------------------------
cross_join <- dplyr::left_join(summary_group, data_group, by = c("region", "date"))


# correlate ---------------------------------------------------------------

cor.test(median_points$med_cross_death, data$deaths_death)
