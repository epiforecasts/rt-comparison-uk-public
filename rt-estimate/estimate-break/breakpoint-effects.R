# Check BP effect sizes


vars <- c("cases", "admissions", "deaths")
models <- c("breakpoint-only",
            "breakpoint-with-rw") #,
regions <- c("South West", "Wales", "Northern Ireland")

# Ridiculous way to get summary because can't quite get purrr to play
regions_fn <- function(model, var, region){
  estimates <- readRDS(paste0("rt-estimate/estimate-break/", 
         model, "/", var, "/region/", region,
         "/2020-11-02/summarised_estimates.rds"))
  estimates$region <- region
  estimates$source <- var
  estimates$model <- model
  estimates <- estimates[estimates$variable == "breakpoints",]
  return(estimates)
  }

m111 <- regions_fn(model = models[1], var = vars[1], region = regions[1])
m112 <- regions_fn(model = models[1], var = vars[1], region = regions[2])
m113 <- regions_fn(model = models[1], var = vars[1], region = regions[3])

m121 <- regions_fn(model = models[1], var = vars[2], region = regions[1])
m122 <- regions_fn(model = models[1], var = vars[2], region = regions[2])
m123 <- regions_fn(model = models[1], var = vars[2], region = regions[3])

m131 <- regions_fn(model = models[1], var = vars[3], region = regions[1])
m132 <- regions_fn(model = models[1], var = vars[3], region = regions[2])
m133 <- regions_fn(model = models[1], var = vars[3], region = regions[3])

m211 <- regions_fn(model = models[2], var = vars[1], region = regions[1])
m212 <- regions_fn(model = models[2], var = vars[1], region = regions[2])
m213 <- regions_fn(model = models[2], var = vars[1], region = regions[3])

m221 <- regions_fn(model = models[2], var = vars[2], region = regions[1])
m222 <- regions_fn(model = models[2], var = vars[2], region = regions[2])
m223 <- regions_fn(model = models[2], var = vars[2], region = regions[3])

m231 <- regions_fn(model = models[2], var = vars[3], region = regions[1])
m232 <- regions_fn(model = models[2], var = vars[3], region = regions[2])
m233 <- regions_fn(model = models[2], var = vars[3], region = regions[3])

summary <- dplyr::bind_rows(m111, m112, m113,
                 m121, m122, m123,
                 m131, m132, m133,
                 m211, m212, m213,
                 m221, m222, m223,
                 m231, m232, m233) %>%
  dplyr::mutate(dplyr::across(median:upper_90, exp))

rm(list = grep("^m", ls(), value = TRUE))


effects <- summary %>%
  dplyr::group_by(region, model) %>%
  dplyr::filter(strat == max(strat)) %>%
  dplyr::select(-c(variable, type, date, strat, mean, sd, lower_50:upper_50)) %>%
  dplyr::mutate(region = factor(region, 
                                levels = c("South West", "Wales", "Northern Ireland"),
                                ordered = TRUE),
                model = factor(model, 
                                levels = c("breakpoint-only", "breakpoint-with-rw"),
                               labels = c("Single breakpoint", "Random walk + breakpoint"),
                                ordered = TRUE))

effects_plot <- ggplot(effects) +
  geom_point(aes(x = median, y = source, colour = model)) +
  geom_errorbarh(aes(xmin = lower_90, xmax = upper_90,
                     y = source, colour = model),
                 alpha = 1) +
  geom_vline(aes(xintercept = 1), lty = 3) +
  facet_wrap(~region) +
  labs(y = NULL, x = "Effect size of breakpoint at firebreak") +
  scale_colour_brewer(type = "qual", palette = 2) +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_rect(colour = "transparent"))

ggsave(here::here("rt-estimate", "estimate-break", 
                  "break-effect-size.png"), 
       height = 3, width = 5)

saveRDS(effects_plot, here::here("rt-estimate", "estimate-break", 
                                 "break-effect-size.rds"))

