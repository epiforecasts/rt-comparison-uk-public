# Plot colours
# Data: data source
colours <- c(
  # Community cases
  "Community" = "#1b9e77",
  "Cases" = "#1b9e77",
  "cases_blend" = "#1b9e77",
  # Admissions
  "Hospital admissions" = "#7570b3",
  "cases_hosp" = "#7570b3",
  # Deaths
  "Deaths" = "#d95f02",
  "deaths_blend" = "#d95f02"
  )

# saveRDS(colours, "colours.rds")


# Order nations and regions
nation_order <- c("England", "Scotland", "Wales", "Northern Ireland")
nhsregion_order <- c("North East and Yorkshire" ,
                "North West",
                "Midlands",
                "East of England",
                "London",
                "South East",
                "South West")
region_names <- list(
  "nations" = factor(nation_order, levels = nation_order, labels = nation_order),
  "nhsregions" = factor(nhsregion_order, levels = nhsregion_order, labels = nhsregion_order),
  "region_factor" = factor(c(nation_order, nhsregion_order),
                           levels = c(nation_order, nhsregion_order))
  )

# saveRDS(region_names, "data/region_names.rds")
 



