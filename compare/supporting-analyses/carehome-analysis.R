# Care home analysis
source("utils/utils.R")
summary <- readRDS("rt-estimate/estimate-all-time/summary_truncated.RDS")



# Deaths in care homes ----------------------------------------------------
# 
# Note deaths are by day of notfication ("2-3 days after death")
# Source: 
# ONS: Number of care home deaths notified to the CQC
# Available at https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/numberofdeathsincarehomesnotifiedtothecarequalitycommissionengland
# Downloaded copy saved under "data/2020-09-27-care-home-deaths.xlsx"

url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fnumberofdeathsincarehomesnotifiedtothecarequalitycommissionengland%2f2020/20200920officialsensitivedeathnotifsv3.xlsx"

httr::GET(url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

sheets <- readxl::excel_sheets(tf)

carehome_la <- readxl::read_excel(path = tf,
                                  sheet = 4,
                                  range = readxl::cell_limits(c(3,1), c(NA, NA))) %>%
  dplyr::rename("local_auth" = "...1") %>%
  tidyr::pivot_longer(-local_auth, names_to = "date", values_to = "deaths") %>%
  dplyr::mutate(date = lubridate::as_date(as.numeric(date), origin = lubridate::origin))

# Convert LAD to regions
la_to_region <- readr::read_csv("https://opendata.arcgis.com/datasets/3ba3daf9278f47daba0f561889c3521a_0.csv") %>%
  dplyr::select("LAD19NM", "RGN19NM")

# fix some missing ones
region_fix <- list(
  "England" = "England",
  "Buckinghamshire" = "South East",
  "Cambridgeshire" = "East of England",
  "Derbyshire" = "East Midlands",
  "Cumbria" = "North West",
  "Devon" = "South West",
  "East Sussex" = "South East",
  "Essex" = "South East",
  "Gloucestershire" = "South West",
  "Hampshire" = "South East",
  "Hertfordshire" = "East of England",
  "Kent" = "South East",
  "Lancashire" = "North West",
  "Leicestershire" = "East Midlands",
  "Lincolnshire" = "Yorkshire and The Humber",
  "Norfolk" = "East of England",
  "North Yorkshire" = "Yorkshire and The Humber",
  "Northamptonshire" = "East Midlands",
  "Nottinghamshire" = "East Midlands",
  "Oxfordshire" = "South East",
  "Somerset" = "South West",
  "Staffordshire" = "West Midlands",
  "Suffolk" = "East of England",
  "Surrey" = "South East",
  "Warwickshire" = "West Midlands",
  "West Sussex" = "South East",
  "Worcestershire" = "West Midlands") %>%
  dplyr::bind_rows() %>%
  t()

region_fix <- tibble::rownames_to_column(as.data.frame(region_fix), var = "LAD19NM") %>%
  dplyr::rename("RGN19NM" = "V1")

la_to_region <- dplyr::bind_rows(la_to_region, region_fix)

# Join to carehome data
carehome_la <- dplyr::left_join(carehome_la, la_to_region, by = c("local_auth" = "LAD19NM")) %>%
  dplyr::filter(!is.na(RGN19NM))

carehome_region <- carehome_la %>%
  dplyr::mutate(region = ifelse(RGN19NM == "East Midlands" | RGN19NM == "West Midlands", 
                                "Midlands", RGN19NM),
                region = ifelse(region == "North East" | RGN19NM == "Yorkshire and The Humber", 
                                "North East and Yorkshire", region)) %>%
  # Summarise deaths data by region
  dplyr::group_by(region, date) %>%
  dplyr::summarise(deaths = sum(deaths), .groups = "drop") %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(ma = TTR::runMean(deaths, 7),
                zsc_ma = scale(ma, center = TRUE, scale = TRUE)) %>%
  dplyr::ungroup() %>%
  # Fix dates - year is 2020!
  dplyr::mutate(year = lubridate::year(Sys.Date()),
                date = lubridate::dmy(paste(lubridate::day(date), 
                                            lubridate::month(date), 
                                            year, sep = "-")))


# Fill out missing dates early in time series
fill_dates <- tibble::tibble(
  region = rep(region_names$region_factor, 
               length(seq.Date(from = as.Date("2020-03-19"), 
                               to = min(carehome_region$date)-1, 
                               by = 1))),
  date = rep(seq.Date(from = as.Date("2020-03-19"), 
                      to = min(carehome_region$date)-1, 
                      by = 1),
             8),
  ma = NA)


# Get all deaths data
source("data/get-uk-data.R")
data_ma_deaths <- data %>%
  dplyr::group_by(region) %>%
  # Moving average of count
  dplyr::mutate(ma = forecast::ma(deaths_death, order = 7),
                Deaths = "All recorded by date of death") %>% 
  dplyr::ungroup() %>%
  dplyr::select(date, region, ma, Deaths, deaths_all = deaths_death)

# Join with care homes data
carehome_region_fill <- dplyr::bind_rows(carehome_region, fill_dates) %>%
  dplyr::mutate(Deaths = "Care home by date of notification") %>%
  dplyr::select(region, date, ma, Deaths, deaths_carehomes = deaths) %>%
  dplyr::bind_rows(data_ma_deaths) %>%
  dplyr::mutate(region = factor(region, levels = region_names$region_factor))

# Regional peaks in death counts
carehome_region_fill %>%
  group_by(region) %>%
  filter(Deaths == "All recorded by date of death",
         deaths_all == max(deaths_all, na.rm=T))
carehome_region_fill %>%
  group_by(region) %>%
  filter(Deaths == "Care home by date of notification",
         deaths_carehomes == max(deaths_carehomes, na.rm=T))


# Plot --------------------------------------------------------------------

date_max <- max(summary$date)
plot_carehome_deaths <- carehome_region_fill %>%
  dplyr::filter(!region %in% "England" & 
                  date <= date_max) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = ma, colour = Deaths), lwd = 1) +
  facet_wrap(~ region, nrow = 2, scales = "free_y") +
  labs(x = NULL, y = "Deaths, 7-day MA") +
  scale_colour_manual(values = colours) +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(size = 15))

ggsave(paste0("figures/", Sys.Date(), "-carehome-deaths.png"),
       height = 6, width = 10)

# # for a quick comparison of the difference that the 2-3 days delay makes:
# carehome_deaths <- carehome_region %>%
#   group_by(region) %>%
#   mutate(lead2 = lead(deaths, 2),
#          lead3 = lead(deaths, 3),
#          lead_mean = (lead2 + lead3) / 2,
#          ma = forecast::ma(lead_mean, order = 7),
#          Deaths = "Average of 2 and 3 day leading deaths") %>%
#   select(region, date, ma, Deaths) %>%
#   bind_rows(data_ma_deaths)
# 
# carehome_deaths %>%
#   ggplot(aes(x = date, colour = Deaths)) +
#   geom_line(aes(y = ma)) +
#   facet_wrap( ~ region, scales = "free_y") +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         strip.background = element_blank(),
#         text = element_text(size = 15))



# Care home admissions to hospitals ---------------------------------------
# 
# adm_carehome <- readxl::read_excel(paste0("data/", Sys.Date(), "-nhs-admissions.xlsx"), 
#                                    sheet = 1,
#                                    range = readxl::cell_limits(c(74, 2), c(82, NA))) %>%
#   t() %>%
#   tibble::as_tibble() %>%
#   janitor::row_to_names(1) %>%
#   dplyr::mutate(date = seq.Date(from = as.Date("2020-08-01"), by = 1, length.out = nrow(.))) %>% # Starts 1 Aug
#   tidyr::pivot_longer(-date, names_to = "region", values_to = "carehome_admissions") %>%
#   dplyr::mutate(carehome_admissions = as.numeric(carehome_admissions))
# 
# adm_carehome %>%
#   dplyr::filter(!region == "ENGLAND") %>%
#   ggplot(aes(x = date, colour = region)) +
#   geom_line(aes(y = forecast::ma(carehome_admissions, 7))) +
#   theme_classic()
# 



# Number of outbreaks in care homes --------------------------------------------------------------
# 
# carehomes <- readxl::read_excel("data/Care_home_outbreaks_of_COVID-19_Management_Information.xlsx", 
#                                 sheet = "PHE_centres",
#                                 range = readxl::cell_limits(c(2, 1), c(NA, NA))) %>%
#   select(-PHEC15CD) %>%
#   pivot_longer(cols = -c('PHE centre', 'Number of care homes'), 
#                names_to = "date", values_to = "outbreaks") %>%
#   mutate(date = as.Date(date)) %>%
#   drop_na(date) %>%
#   rename(region=1, n_carehomes=2, date=3, outbreaks=4)
# 
# 
# n_carehomes <- as.data.frame(distinct(carehomes, region, n_carehomes))
# Midlands <- data.frame(region = "Midlands")
# Midlands$n_carehomes <- n_carehomes$n_carehomes[n_carehomes$region == "East Midlands"] +
#   n_carehomes$n_carehomes[n_carehomes$region == "West Midlands"]
# NEY <- data.frame(region = "North East and Yorkshire")
# NEY$n_carehomes <- n_carehomes[n_carehomes$region == "Yorkshire and Humber","n_carehomes"] +
#   n_carehomes[n_carehomes$region == "North East","n_carehomes"]
# n_carehomes <- bind_rows(n_carehomes, Midlands, NEY)
# n_carehomes <- n_carehomes[n_carehomes$region %in% region_names$value$nhsregions,]
# 
# region_n <- c("North East and Yorkshire (n=2238)",
#               "North West (n=1917)",
#               "Midlands (n=3227)",
#               "East of England (n=1726)",
#               "London (n=1385)",
#               "South East (n=2942)",
#               "South West (n=2041)")
# region_n_factor <- factor(region_n, levels = region_n)
# 
# carehomes_outbreaks <- carehomes %>%
#   pivot_wider(-n_carehomes, names_from = region, values_from = outbreaks) %>%
#   mutate(Midlands = `East Midlands` + `West Midlands`,
#          `North East and Yorkshire` = `Yorkshire and Humber` + `North East`) %>%
#   select(date, all_of(region_names$value$nhsregions)) %>%
#   pivot_longer(-date,  names_to = "region", values_to = "outbreaks") %>%
#   left_join(n_carehomes, by = "region") %>%
#   mutate(perc_outbreak = outbreaks / n_carehomes *100,
#          region = factor(region, region_names$value$region_factor),
#          region_n = factor(paste0(region, " (n=", n_carehomes, ")"), levels = region_n_factor))
# 
# full_dates <- dplyr::filter(data, region %in% region_names$value$nhsregions & 
#                               date >= min(carehomes_outbreaks$date) &
#                               date <= max(carehomes_outbreaks$date)) %>%
#   select(date, region) %>%
#   left_join(carehomes_outbreaks, by = c("date", "region")) %>%
#   group_by(region) %>%
#   fill(c(n_carehomes, perc_outbreak), .direction = "down") %>%
#   left_join(spring_peaks, by = c("date", "region")) %>%
#   mutate(peak_adm = ifelse(source == "hosp", format(as.Date(date, "%m/%d/%Y")), NA),
#          peak_deaths = ifelse(source == "deaths", format(as.Date(date, "%m/%d/%Y")), NA),
#          region = factor(region, region_names$value$region_factor),
#          region_n = factor(region_n, region_n_factor)) 
# 
# # Plot
# plot_carehomes <- carehomes_outbreaks %>%
#   ggplot() +
#   geom_line(aes(x = date, y = perc_outbreak)) +
#   geom_vline(aes(xintercept = as.Date(peak_adm)), data = full_dates, lty = "dotted", lwd=1) +
#   geom_vline(aes(xintercept = as.Date(peak_deaths)), data = full_dates, lty = "dashed", lwd = 1) +
#   # geom_point(aes(x = peak_adm, y = perc_outbreak), data = full_dates, shape = 2) +
#   # geom_point(aes(x = peak_deaths, y = perc_outbreak), data = full_dates, shape = 3) +
#   facet_wrap("region_n", nrow = 2, scales = "free_y") +
#   cowplot::theme_cowplot() +
#   coord_cartesian(xlim = c(date_min, max(carehomes_outbreaks$date))) +
#   scale_color_manual(values = colours) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b") +
#   theme(panel.spacing.x = unit(0.1, "cm"),
#         panel.spacing.y = unit(0.1, "cm"),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         text = element_text(size = 15)) +
#   # Add label with N
#   guides(colour = FALSE) +
#   labs(y = "% care homes with new outbreak", x = NULL)
# 
# ggsave("figures/carehome_outbreaks.png", height = 4, width = 10)
# 
# 
# 
